package com.webforj.devtools.livereload.receiver;

import com.webforj.devtools.livereload.LiveReloadServer;
import com.webforj.plugin.foundation.WatchPortFile;
import com.webforj.plugin.foundation.WatchProtocol;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Connects to the development watch socket and turns its messages into browser reloads.
 *
 * <p>
 * The watch runs in the stable Maven process and forwards two kinds of line over the socket. A log
 * line carries one line of bundler output, which this receiver re-logs so it appears in the
 * application log. A rebuild line carries the changed served paths, which this receiver classifies
 * into a stylesheet hot swap or a full page reload and pushes through the reload server it was
 * given. Without hot swap by url, a stylesheet only rebuild is pushed with the stylesheet content
 * read from the classpath.
 * </p>
 *
 * <p>
 * The owning lifecycle creates one receiver, starts it, and stops it on a context teardown. The
 * receiver reads the discovery file to learn the socket port, connects, and reconnects whenever the
 * connection drops, so it survives a watch restart and a stale discovery file alike. A receiver
 * created with a watch end listener instead stops and tells the listener when the watch closes a
 * connection the receiver had.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public class WatchReceiver implements LiveReloadReceiver {

  // Uses the bundler's own slf4j logger name so the re-logged watch output lands in the application
  // log under the same namespace and configuration as the bundler itself. A System.Logger would
  // route to JUL and could not carry that name.
  private static final Logger logger = LoggerFactory.getLogger("com.webforj.bundle.bun");
  private static final String CSS_EXTENSION = ".css";
  private static final String SERVED_DIRECTORY = "static/frontend/";
  private static final Set<String> IMAGE_EXTENSIONS =
      Set.of(".png", ".jpg", ".jpeg", ".gif", ".svg", ".webp", ".ico");
  private static final int CONNECT_TIMEOUT_MILLIS = 1000;
  private static final long RETRY_DELAY_MILLIS = 1000;

  private final LiveReloadServer server;
  private final boolean staticResourcesEnabled;
  private final String projectRoot;
  private final Runnable watchEndListener;
  private final AtomicBoolean running = new AtomicBoolean(false);
  private final AtomicReference<Socket> socket = new AtomicReference<>();
  private final AtomicReference<Thread> connector = new AtomicReference<>();

  /**
   * Creates a receiver that pushes the watch changes through the given reload server.
   *
   * @param server the reload server the changes are pushed through
   * @param staticResourcesEnabled whether a stylesheet or image change hot swaps rather than
   *        reloads
   */
  public WatchReceiver(LiveReloadServer server, boolean staticResourcesEnabled) {
    this(server, staticResourcesEnabled, System.getProperty("user.dir"), null);
  }

  /**
   * Creates a receiver for the development watch of the given project that reconnects whenever the
   * watch ends.
   *
   * @param server the reload server the changes are pushed through
   * @param staticResourcesEnabled whether a stylesheet or image change hot swaps by url rather than
   *        being pushed as content or reloaded
   * @param projectRoot the absolute root directory of the project the watch runs in
   *
   * @since 26.03
   */
  public WatchReceiver(LiveReloadServer server, boolean staticResourcesEnabled,
      String projectRoot) {
    this(server, staticResourcesEnabled, projectRoot, null);
  }

  /**
   * Creates a receiver for the development watch of the given project that tells the listener when
   * the watch it connected to ends.
   *
   * @param server the reload server the changes are pushed through
   * @param staticResourcesEnabled whether a stylesheet or image change hot swaps by url rather than
   *        being pushed as content or reloaded
   * @param projectRoot the absolute root directory of the project the watch runs in
   * @param watchEndListener told once the watch ended
   *
   * @since 26.03
   */
  public WatchReceiver(LiveReloadServer server, boolean staticResourcesEnabled, String projectRoot,
      Runnable watchEndListener) {
    this.server = server;
    this.staticResourcesEnabled = staticResourcesEnabled;
    this.projectRoot = projectRoot;
    this.watchEndListener = watchEndListener;
  }

  /**
   * Starts the connector thread, unless the receiver is already running.
   */
  public void start() {
    if (!running.compareAndSet(false, true)) {
      return;
    }

    Thread thread = new Thread(this::connectLoop, "webforj-watch-receiver");
    thread.setDaemon(true);
    connector.set(thread);
    thread.start();
  }

  /**
   * Stops the receiver, drops its connection, and interrupts the connector thread.
   */
  public void stop() {
    running.set(false);

    Socket current = socket.getAndSet(null);
    if (current != null) {
      try {
        current.close();
      } catch (IOException e) {
        // closing a dead socket has nothing left to recover
      }
    }

    Thread thread = connector.getAndSet(null);
    if (thread != null) {
      thread.interrupt();
    }
  }

  /**
   * Indicates whether the receiver is running.
   *
   * @return {@code true} between a start and a stop
   */
  public boolean isRunning() {
    return running.get();
  }

  private void connectLoop() {
    while (running.get()) {
      Path portFile = WatchPortFile.resolve(projectRoot);
      Integer port = readPort(portFile);
      if (port != null && consume(port) && watchEndListener != null
          && running.compareAndSet(true, false)) {
        logger.info("the webforj watch ended, the development run ends with it");
        watchEndListener.run();
        return;
      }

      sleep();
    }
  }

  private boolean consume(int port) {
    boolean connected = false;
    try (Socket connection = new Socket()) {
      connection.connect(new InetSocketAddress("127.0.0.1", port), CONNECT_TIMEOUT_MILLIS);
      connected = true;
      socket.set(connection);
      logger.debug("connected to the webforj watch on port {}", port);
      readLines(connection);
    } catch (IOException e) {
      logger.debug("watch socket not reachable on port {}: {}", port, e.getMessage());
    } finally {
      socket.set(null);
    }

    return connected;
  }

  private void readLines(Socket connection) throws IOException {
    BufferedReader reader = new BufferedReader(
        new InputStreamReader(connection.getInputStream(), StandardCharsets.UTF_8));
    String line;
    while (running.get() && (line = reader.readLine()) != null) {
      handleLine(line);
    }
  }

  void handleLine(String line) {
    if (line.startsWith(WatchProtocol.LOG_PREFIX)) {
      if (logger.isInfoEnabled()) {
        logger.info(line.substring(WatchProtocol.LOG_PREFIX.length()));
      }
    } else if (line.startsWith(WatchProtocol.WARN_PREFIX)) {
      if (logger.isWarnEnabled()) {
        logger.warn(line.substring(WatchProtocol.WARN_PREFIX.length()));
      }
    } else if (line.startsWith(WatchProtocol.REBUILD_PREFIX)) {
      String paths = line.substring(WatchProtocol.REBUILD_PREFIX.length()).trim();

      if (!paths.isEmpty()) {
        pushReload(List.of(paths.split(" ")));
      }
    }
  }

  void pushReload(List<String> changedPaths) {
    if (server == null || !server.isRunning()) {
      return;
    }

    boolean anyUnswappable = changedPaths.stream().anyMatch(path -> !isSwappable(path));
    if (anyUnswappable) {
      server.sendReloadMessage();
      return;
    }

    if (!staticResourcesEnabled) {
      sendStylesheetContents(changedPaths);
      return;
    }

    for (String path : changedPaths) {
      server.sendResourceUpdateMessage(getResourceType(path), path, null);
    }
  }

  private static boolean isSwappable(String path) {
    String lower = path.toLowerCase(Locale.ROOT);
    return lower.endsWith(CSS_EXTENSION) || IMAGE_EXTENSIONS.stream().anyMatch(lower::endsWith);
  }

  private static String getResourceType(String path) {
    return isStylesheet(path) ? "css" : "image";
  }

  static Integer readPort(Path portFile) {
    if (portFile == null || !Files.isRegularFile(portFile)) {
      return null;
    }

    try {
      return Integer.parseInt(Files.readString(portFile, StandardCharsets.UTF_8).trim());
    } catch (IOException | NumberFormatException e) {
      return null;
    }
  }

  private void sendStylesheetContents(List<String> changedPaths) {
    Map<String, String> contents = new LinkedHashMap<>();
    for (String path : changedPaths) {
      String content = isStylesheet(path) ? readStylesheet(path) : null;
      if (content == null) {
        server.sendReloadMessage();
        return;
      }

      contents.put(path, content);
    }

    contents.forEach((path, content) -> server.sendResourceUpdateMessage("css", path, content));
  }

  private static String readStylesheet(String path) {
    try (InputStream stream =
        WatchReceiver.class.getClassLoader().getResourceAsStream(SERVED_DIRECTORY + path)) {
      return stream == null ? null : new String(stream.readAllBytes(), StandardCharsets.UTF_8);
    } catch (IOException e) {
      return null;
    }
  }

  private static boolean isStylesheet(String path) {
    return path.toLowerCase(Locale.ROOT).endsWith(CSS_EXTENSION);
  }

  private void sleep() {
    if (!running.get()) {
      return;
    }

    try {
      Thread.sleep(RETRY_DELAY_MILLIS);
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();
      running.set(false);
    }
  }
}
