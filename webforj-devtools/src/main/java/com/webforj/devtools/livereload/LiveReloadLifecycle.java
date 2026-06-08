package com.webforj.devtools.livereload;

/**
 * Owns the reload server and the watch receiver for one container context.
 *
 * <p>
 * Each runtime contributes a thin adapter that calls {@link #start(LiveReloadOptions)} when its
 * context comes up and {@link #stop()} when the context is torn down. The lifecycle creates the
 * reload server and the receiver, hands the server to the receiver, and tears both down together,
 * so the next context binds the reload port cleanly and runs a single receiver.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public final class LiveReloadLifecycle {

  private static final System.Logger logger = System.getLogger(LiveReloadLifecycle.class.getName());

  private LiveReloadServer server;
  private WatchReceiver receiver;

  /**
   * Brings the reload server and the receiver up, unless live reload is off or already running.
   *
   * @param options the live reload configuration
   */
  public synchronized void start(LiveReloadOptions options) {
    if (!options.isEnabled() || server != null) {
      return;
    }

    LiveReloadServer reloadServer = new LiveReloadServer(options.getWebsocketPort());
    reloadServer.start();

    WatchReceiver watchReceiver =
        new WatchReceiver(reloadServer, options.isStaticResourcesEnabled());
    watchReceiver.start();

    this.server = reloadServer;
    this.receiver = watchReceiver;
    logger.log(System.Logger.Level.INFO,
        "webforJ live reload ready on port " + options.getWebsocketPort());
  }

  /**
   * Stops the receiver and the reload server, releasing the reload port.
   */
  public synchronized void stop() {
    if (receiver != null) {
      receiver.stop();
      receiver = null;
    }

    if (server != null) {
      try {
        server.stop();
      } catch (InterruptedException e) {
        Thread.currentThread().interrupt();
      } finally {
        server = null;
      }
    }
  }

  /**
   * Indicates whether the reload server is running.
   *
   * @return {@code true} when the reload server is up
   */
  public synchronized boolean isRunning() {
    return server != null && server.isRunning();
  }
}
