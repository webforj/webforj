package com.webforj.devtools.livereload;

import com.webforj.devtools.livereload.receiver.HotswapAgentReceiver;
import com.webforj.devtools.livereload.receiver.JrebelReceiver;
import com.webforj.devtools.livereload.receiver.LiveReloadReceiver;
import com.webforj.devtools.livereload.receiver.WatchReceiver;
import java.util.ArrayList;
import java.util.List;

/**
 * Owns the reload server and its receivers for one container context.
 *
 * <p>
 * Each runtime contributes a thin adapter that calls {@link #start(LiveReloadOptions)} when its
 * context comes up and {@link #stop()} when the context is torn down. The lifecycle creates the
 * reload server, hands it to every receiver, and tears everything down together, so the next
 * context binds the reload port cleanly and runs a single set of receivers. When the options ask
 * for it, the lifecycle also stops when the watch it is connected to ends.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public final class LiveReloadLifecycle {

  private static final System.Logger logger = System.getLogger(LiveReloadLifecycle.class.getName());

  private final List<LiveReloadReceiver> receivers = new ArrayList<>();
  private final List<ClassLoader> openClassLoaders = new ArrayList<>();
  private LiveReloadServer server;
  private boolean notifiedRestarting;

  /**
   * Brings the reload server and the receiver up, unless live reload is off or already running.
   *
   * @param options the live reload configuration
   *
   * @throws RuntimeException when a receiver fails to start, after everything started is stopped
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
    this.server = reloadServer;
    this.notifiedRestarting = false;

    try {
      List<LiveReloadReceiver> created = List.of(createWatchReceiver(reloadServer, options),
          new JrebelReceiver(reloadServer), new HotswapAgentReceiver(reloadServer));
      for (LiveReloadReceiver receiver : created) {
        receiver.start();
        receivers.add(receiver);
      }
    } catch (RuntimeException e) {
      try {
        stop();
      } catch (RuntimeException stopFailure) {
        e.addSuppressed(stopFailure);
      }

      throw e;
    }

    logger.log(System.Logger.Level.INFO,
        "webforJ live reload ready on port " + options.getWebsocketPort());
  }

  /**
   * Stops the receivers and the reload server, releasing the reload port and every class loader
   * opened since the start.
   *
   * @throws RuntimeException the first receiver stop failure, after every receiver and the server
   *         were stopped
   */
  public synchronized void stop() {
    for (ClassLoader loader : openClassLoaders) {
      receivers.forEach(receiver -> receiver.closeClassLoader(loader));
    }

    openClassLoaders.clear();

    RuntimeException failure = null;
    for (LiveReloadReceiver receiver : receivers) {
      try {
        receiver.stop();
      } catch (RuntimeException e) {
        logger.log(System.Logger.Level.WARNING,
            "Could not stop the live reload receiver " + receiver.getClass().getName(), e);
        if (failure == null) {
          failure = e;
        }
      }
    }

    receivers.clear();

    if (server != null) {
      try {
        server.stop();
      } catch (InterruptedException e) {
        Thread.currentThread().interrupt();
      } finally {
        server = null;
      }
    }

    if (failure != null) {
      throw failure;
    }
  }

  /**
   * Prepares every receiver for an application class loader that starts serving sessions, while the
   * lifecycle is running.
   *
   * @param loader the application class loader
   *
   * @since 26.03
   */
  public synchronized void openClassLoader(ClassLoader loader) {
    if (server == null || openClassLoaders.contains(loader)) {
      return;
    }

    openClassLoaders.add(loader);
    receivers.forEach(receiver -> receiver.openClassLoader(loader));
  }

  /**
   * Releases an application class loader from every receiver after its last session ended, while
   * the lifecycle is running.
   *
   * @param loader the application class loader
   *
   * @since 26.03
   */
  public synchronized void closeClassLoader(ClassLoader loader) {
    if (openClassLoaders.remove(loader)) {
      receivers.forEach(receiver -> receiver.closeClassLoader(loader));
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

  /**
   * Gets the receiver that listens for hotswap class reload events, so a test can assert that the
   * lifecycle owns it.
   *
   * @return the receiver while the lifecycle is running, {@code null} otherwise
   */
  JrebelReceiver getJrebelReceiver() {
    return findReceiver(JrebelReceiver.class);
  }

  /**
   * Gets the receiver that listens for HotswapAgent class redefinitions, so a test can assert that
   * the lifecycle owns it.
   *
   * @return the receiver while the lifecycle is running, {@code null} otherwise
   */
  HotswapAgentReceiver getHotswapAgentReceiver() {
    return findReceiver(HotswapAgentReceiver.class);
  }

  private WatchReceiver createWatchReceiver(LiveReloadServer reloadServer,
      LiveReloadOptions options) {
    if (options.isStopOnWatchEnd()) {
      return new WatchReceiver(reloadServer, options.isStaticResourcesEnabled(),
          options.getProjectRoot(), () -> handleWatchEnd(reloadServer));
    }

    return new WatchReceiver(reloadServer, options.isStaticResourcesEnabled(),
        options.getProjectRoot());
  }

  private synchronized void handleWatchEnd(LiveReloadServer endedServer) {
    if (server == endedServer) {
      logger.log(System.Logger.Level.INFO,
          "webforJ live reload stops, the watch of the development run ended");
      stop();
    }
  }

  private <T extends LiveReloadReceiver> T findReceiver(Class<T> type) {
    for (LiveReloadReceiver receiver : receivers) {
      if (type.isInstance(receiver)) {
        return type.cast(receiver);
      }
    }

    return null;
  }
}
