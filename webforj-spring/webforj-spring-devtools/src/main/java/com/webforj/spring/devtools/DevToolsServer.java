package com.webforj.spring.devtools;

import com.google.gson.Gson;
import com.webforj.spring.devtools.message.ConnectedMessage;
import com.webforj.spring.devtools.message.HeartbeatAckMessage;
import com.webforj.spring.devtools.message.ReloadMessage;
import com.webforj.spring.devtools.message.ResourceUpdateMessage;
import java.net.InetSocketAddress;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import org.java_websocket.WebSocket;
import org.java_websocket.handshake.ClientHandshake;
import org.java_websocket.server.WebSocketServer;

/**
 * WebSocket server that manages browser connections for automatic page reload during Spring
 * DevTools restarts.
 *
 * <p>
 * This server runs on a separate port from the main application to persist across DevTools
 * restarts. It maintains active WebSocket connections from browser clients and broadcasts reload
 * commands when the application restarts.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.02
 */
public class DevToolsServer extends WebSocketServer {
  private static final System.Logger logger = System.getLogger(DevToolsServer.class.getName());
  private final Set<WebSocket> connections = ConcurrentHashMap.newKeySet();
  private volatile boolean serverClosed = false;
  private final Gson gson = new Gson();

  /**
   * Creates a new DevTools WebSocket server on the specified port.
   *
   * @param port the port number to listen on (typically 35730)
   */
  public DevToolsServer(int port) {
    super(new InetSocketAddress(port));
    setReuseAddr(true);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void onOpen(WebSocket conn, ClientHandshake handshake) {
    connections.add(conn);
    logger.log(System.Logger.Level.INFO,
        "WebSocket client connected. Total connections: " + connections.size());

    // Send initial handshake confirmation
    conn.send(gson.toJson(new ConnectedMessage()));
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void onClose(WebSocket conn, int code, String reason, boolean remote) {
    connections.remove(conn);
    logger.log(System.Logger.Level.INFO, "WebSocket client disconnected. Reason: " + reason
        + ", Total connections: " + connections.size());
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
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void onError(WebSocket conn, Exception ex) {
    logger.log(System.Logger.Level.ERROR, "WebSocket error", ex);
    if (conn != null) {
      connections.remove(conn);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void onStart() {
    serverClosed = false;
    logger.log(System.Logger.Level.INFO,
        "webforJ DevTools server started on port " + getPort());
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void stop() throws InterruptedException {
    serverClosed = true;
    super.stop();
  }

  /**
   * Checks whether the server is currently running.
   *
   * @return true if the server is running, false if it has been stopped
   */
  public boolean isOpen() {
    return !serverClosed;
  }

  /**
   * Gets the current number of active WebSocket connections.
   *
   * @return the count of open connections
   */
  public int getConnectionCount() {
    return connections.size();
  }

  /**
   * Broadcasts a reload command to all connected browser clients.
   *
   * <p>
   * This is called when Spring DevTools triggers an application restart. Cleans up dead connections
   * before sending to ensure accurate counts.
   * </p>
   */
  public void sendReloadMessage() {
    // Clean up dead connections first
    cleanupConnections();

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
   * Broadcasts a resource update to all connected browser clients.
   *
   * <p>
   * This is called when Spring DevTools detects changes to static resources like CSS, JavaScript,
   * or images. The browser clients can then update these resources without a full page reload.
   * </p>
   *
   * @param resourceType the type of resource (css, js, image)
   * @param path the resource path relative to the static folder
   * @param content the resource content (for CSS files) or null for other types
   */
  public void sendResourceUpdateMessage(String resourceType, String path, String content) {
    ResourceUpdateMessage message = new ResourceUpdateMessage(resourceType, path, content);
    String json = gson.toJson(message);

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
   * Removes any WebSocket connections that are no longer open from the active set. Called before
   * broadcasting messages to avoid sending to dead connections.
   */
  private void cleanupConnections() {
    connections.removeIf(conn -> !conn.isOpen());
  }
}
