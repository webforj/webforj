package com.webforj.devtools.livereload;

import com.google.gson.Gson;
import com.google.gson.JsonSyntaxException;
import com.webforj.devtools.livereload.message.ClassUpdateMessage;
import com.webforj.devtools.livereload.message.ConnectedMessage;
import com.webforj.devtools.livereload.message.HeartbeatAckMessage;
import com.webforj.devtools.livereload.message.HelloMessage;
import com.webforj.devtools.livereload.message.ReloadMessage;
import com.webforj.devtools.livereload.message.ResourceUpdateMessage;
import com.webforj.devtools.livereload.message.RestartingMessage;
import java.net.InetSocketAddress;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;
import org.java_websocket.WebSocket;
import org.java_websocket.handshake.ClientHandshake;
import org.java_websocket.server.WebSocketServer;

/**
 * A separate port socket that broadcasts reload commands to the connected browsers.
 *
 * <p>
 * The owning lifecycle creates one instance, starts it, holds it, and stops it on a context
 * teardown. The receiver feeds it the changes that come from the watch, which it turns into a
 * stylesheet hot swap or a full page reload for every connected browser.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public class LiveReloadServer extends WebSocketServer {
  private static final System.Logger logger = System.getLogger(LiveReloadServer.class.getName());

  private final Set<WebSocket> connections = ConcurrentHashMap.newKeySet();
  private final Map<String, ResourceUpdateMessage> lastResourceUpdates = new ConcurrentHashMap<>();
  private final Map<String, Long> lastClassUpdates = new ConcurrentHashMap<>();
  private final Gson gson = new Gson();
  private volatile boolean running = false;
  private volatile boolean started = false;
  private volatile long lastReloadCommandAt = 0;

  /**
   * Creates a reload server on the given port.
   *
   * @param port the port the server listens on
   */
  public LiveReloadServer(int port) {
    super(new InetSocketAddress(port));
    setReuseAddr(true);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void start() {
    running = true;
    super.start();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void stop() throws InterruptedException {
    running = false;
    super.stop();
  }

  /**
   * Indicates whether the server is running.
   *
   * @return {@code true} between a start and a stop
   */
  public boolean isRunning() {
    return running;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void onOpen(WebSocket conn, ClientHandshake handshake) {
    connections.add(conn);
    logger.log(System.Logger.Level.DEBUG,
        "webforJ livereload client connected. Total connections: " + connections.size());

    conn.send(gson.toJson(new ConnectedMessage()));
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void onClose(WebSocket conn, int code, String reason, boolean remote) {
    connections.remove(conn);
    logger.log(System.Logger.Level.DEBUG, "webforJ livereload client disconnected. Reason: "
        + reason + ", Total connections: " + connections.size());
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void onMessage(WebSocket conn, String message) {
    logger.log(System.Logger.Level.DEBUG, "Received message: " + message);
    if ("ping".equals(message)) {
      conn.send(gson.toJson(new HeartbeatAckMessage()));
      logger.log(System.Logger.Level.DEBUG, "Sent heartbeat-ack");
      return;
    }

    handleHello(conn, message);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void onError(WebSocket conn, Exception ex) {
    logger.log(System.Logger.Level.ERROR, "webforJ livereload socket error", ex);
    if (conn != null) {
      connections.remove(conn);
    } else if (!started) {
      // a server level error before the port is bound, such as the port already being in use,
      // means the server never started running
      running = false;
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void onStart() {
    started = true;
    logger.log(System.Logger.Level.INFO, "webforJ livereload server started on port " + getPort());
  }

  /**
   * Gets the current number of active connections.
   *
   * @return the count of open connections
   */
  public int getConnectionCount() {
    return connections.size();
  }

  /**
   * Broadcasts a reload command to every connected browser.
   *
   * <p>
   * The dead connections are dropped first, so the count the broadcast reports stays accurate.
   * </p>
   */
  public void sendReloadMessage() {
    cleanupConnections();
    // The stamp survives the broadcast, so a browser that is between pages right now still learns
    // about this command when its next page connects and reports an older served time.
    lastReloadCommandAt = System.currentTimeMillis();

    logger.log(System.Logger.Level.INFO,
        "Triggering browser reload for " + connections.size() + " connected sessions");

    int successCount = 0;
    for (WebSocket conn : connections) {
      if (conn.isOpen()) {
        try {
          conn.send(gson.toJson(new ReloadMessage()));
          successCount++;
          logger.log(System.Logger.Level.DEBUG, "Sent reload message to client");
        } catch (Exception e) {
          logger.log(System.Logger.Level.ERROR, "Error sending reload message", e);
          connections.remove(conn);
        }
      }
    }

    logger.log(System.Logger.Level.INFO,
        "Successfully sent reload message to " + successCount + " clients");
  }

  /**
   * Broadcasts a restart notice to every connected browser.
   *
   * <p>
   * The browsers keep their page as it is while the server goes down and reload it once the server
   * answers again.
   * </p>
   */
  public void sendRestartingMessage() {
    cleanupConnections();

    logger.log(System.Logger.Level.INFO,
        "Notifying " + connections.size() + " connected sessions about the server restart");

    for (WebSocket conn : connections) {
      if (conn.isOpen()) {
        try {
          conn.send(gson.toJson(new RestartingMessage()));
          logger.log(System.Logger.Level.DEBUG, "Sent restarting message to client");
        } catch (Exception e) {
          logger.log(System.Logger.Level.ERROR, "Error sending restarting message", e);
          connections.remove(conn);
        }
      }
    }
  }

  /**
   * Broadcasts a resource update to every connected browser.
   *
   * <p>
   * A stylesheet or image change is applied in place by the browser without a full page reload.
   * </p>
   *
   * @param resourceType the resource type, such as css or image
   * @param path the resource path relative to the served directory
   * @param content the resource content for a stylesheet, or {@code null} for other types
   */
  public void sendResourceUpdateMessage(String resourceType, String path, String content) {
    ResourceUpdateMessage message = new ResourceUpdateMessage(resourceType, path, content);
    String json = gson.toJson(message);

    // The latest update per path is kept, so a page that was between pages when this broadcast
    // went out still receives it on its next connection and applies it in place.
    lastResourceUpdates.put(path, message);

    cleanupConnections();

    logger.log(System.Logger.Level.INFO, "Sending resource update (" + resourceType + ": " + path
        + ") to " + connections.size() + " connected sessions");

    int successCount = 0;
    for (WebSocket conn : connections) {
      if (conn.isOpen()) {
        try {
          conn.send(json);
          successCount++;
        } catch (Exception e) {
          logger.log(System.Logger.Level.ERROR, "Error sending resource update", e);
          connections.remove(conn);
        }
      }
    }

    logger.log(System.Logger.Level.INFO,
        "Successfully sent resource update to " + successCount + " clients");
  }

  /**
   * Broadcasts a class update to every connected browser.
   *
   * <p>
   * The message names the redefined Java classes, so each page can ask its application instance to
   * rebuild exactly the part of the interface those classes drive instead of reloading the whole
   * page.
   * </p>
   *
   * @param classNames the binary names of the redefined classes
   *
   * @since 26.02
   */
  public void sendClassUpdateMessage(Set<String> classNames) {
    if (classNames == null || classNames.isEmpty()) {
      return;
    }

    ClassUpdateMessage message = new ClassUpdateMessage(List.copyOf(classNames));
    String json = gson.toJson(message);

    // The latest update per class is kept, so a page that was between pages when this broadcast
    // went out still receives the classes it missed on its next connection.
    for (String className : classNames) {
      lastClassUpdates.put(className, message.getTimestamp());
    }

    cleanupConnections();

    logger.log(System.Logger.Level.INFO, "Sending class update (" + String.join(", ", classNames)
        + ") to " + connections.size() + " connected sessions");

    for (WebSocket conn : connections) {
      if (conn.isOpen()) {
        try {
          conn.send(json);
        } catch (Exception e) {
          logger.log(System.Logger.Level.ERROR, "Error sending class update", e);
          connections.remove(conn);
        }
      }
    }
  }

  /**
   * Catches a connecting page up on everything it missed while its browser was between pages.
   *
   * <p>
   * A broadcast that fires while a browser is between pages reaches nobody and would otherwise be
   * lost, leaving that browser on stale content. The client reports the server clock time its page
   * was served with, and a page that already applied a resource update in place advances that
   * stamp, so the comparison always names exactly what the page missed. A missed reload command
   * gets the one reload it missed. A missed resource update is replayed to the page and applies in
   * place, the same way the live broadcast would have, so a stylesheet change never turns into a
   * page reload. A page with a newer stamp is never sent anything, so the catch up always ends.
   * </p>
   */
  private void handleHello(WebSocket conn, String message) {
    if (message == null || !message.startsWith("{")) {
      return;
    }

    HelloMessage hello;
    try {
      hello = gson.fromJson(message, HelloMessage.class);
    } catch (JsonSyntaxException e) {
      logger.log(System.Logger.Level.DEBUG, "Ignoring an unreadable client message", e);
      return;
    }

    if (hello == null || !HelloMessage.TYPE.equals(hello.getType())
        || hello.getPageServedAt() <= 0) {
      return;
    }

    if (lastReloadCommandAt > hello.getPageServedAt()) {
      logger.log(System.Logger.Level.INFO,
          "A page served before the last reload command connected, reloading it");
      conn.send(gson.toJson(new ReloadMessage()));

      // The reloaded page fetches every resource fresh, so nothing else needs replaying.
      return;
    }

    for (ResourceUpdateMessage update : lastResourceUpdates.values()) {
      if (update.getTimestamp() > hello.getPageServedAt()) {
        logger.log(System.Logger.Level.INFO, "Replaying a missed resource update ("
            + update.getResourceType() + ": " + update.getPath() + ") to a connecting page");
        conn.send(gson.toJson(update));
      }
    }

    Set<String> missedClasses = new TreeSet<>();
    for (Map.Entry<String, Long> update : lastClassUpdates.entrySet()) {
      if (update.getValue() > hello.getPageServedAt()) {
        missedClasses.add(update.getKey());
      }
    }

    if (!missedClasses.isEmpty()) {
      logger.log(System.Logger.Level.INFO, "Replaying a missed class update ("
          + String.join(", ", missedClasses) + ") to a connecting page");
      conn.send(gson.toJson(new ClassUpdateMessage(List.copyOf(missedClasses))));
    }
  }

  private void cleanupConnections() {
    connections.removeIf(conn -> !conn.isOpen());
  }
}
