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
  private boolean notifiedRestarting;

  /**
   * Brings the reload server and the receiver up, unless live reload is off or already running.
   *
   * @param options the live reload configuration
   */
  public synchronized void start(LiveReloadOptions options) {
    if (!options.isEnabled()) {
      logger.log(System.Logger.Level.INFO, "webforJ live reload is disabled and will not start");

      return;
    }

    if (server != null) {
      return;
    }

    LiveReloadServer reloadServer = new LiveReloadServer(options.getWebsocketPort());
    reloadServer.start();

    WatchReceiver watchReceiver =
        new WatchReceiver(reloadServer, options.isStaticResourcesEnabled());
    watchReceiver.start();

    this.server = reloadServer;
    this.receiver = watchReceiver;
    this.notifiedRestarting = false;
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
   * Tells every connected browser that the server is about to restart.
   *
   * <p>
   * The notice must go out before the sessions are torn down, so the browsers keep their page as it
   * is during the restart instead of reacting to the session end. A teardown reaches this method
   * more than once when several teardown callbacks fire for the same restart, so only the first
   * call since the last start sends the notice.
   * </p>
   *
   * @since 26.02
   */
  public synchronized void notifyRestarting() {
    if (server != null && !notifiedRestarting) {
      notifiedRestarting = true;
      server.sendRestartingMessage();
    }
  }

  /**
   * Sends a resource update to every connected browser, so a stylesheet or image change applies in
   * place without a page reload.
   *
   * @param resourceType the resource type, {@code css}, {@code js}, {@code image}, or {@code other}
   * @param path the served resource path
   *
   * @since 26.02
   */
  public synchronized void sendResourceUpdate(String resourceType, String path) {
    if (server != null) {
      server.sendResourceUpdateMessage(resourceType, path, null);
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
