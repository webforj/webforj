package com.webforj.devtools.livereload;

import com.typesafe.config.Config;
import com.webforj.App;
import com.webforj.AppLifecycleListener;
import com.webforj.Environment;
import com.webforj.devtools.ProjectRootResolver;
import com.webforj.utilities.Assets;

/**
 * Runs live reload for an app in BBjServices, from its first session to its last.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class LiveReloadBBjLifecycle implements AppLifecycleListener {

  private static final ClassLoader APP_LOADER = LiveReloadBBjLifecycle.class.getClassLoader();
  private static final SharedLifecycle SHARED = new SharedLifecycle(new LiveReloadLifecycle());

  private final SharedLifecycle lifecycle;
  private boolean started;

  /**
   * Creates the listener on the live reload shared by the sessions of the app.
   */
  public LiveReloadBBjLifecycle() {
    this(SHARED);
  }

  LiveReloadBBjLifecycle(SharedLifecycle lifecycle) {
    this.lifecycle = lifecycle;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void onWillRun(App app) {
    if (Environment.isRunningWithBBjServices()) {
      Config config = getConfig();
      LiveReloadOptions options = LiveReloadOptions.from(config).setStopOnWatchEnd(true)
          .setProjectRoot(ProjectRootResolver.resolve(config, app.getClass()).toString());
      if (config != null && !Assets.isPackagedOnBBj()) {
        options.setStaticResourcesEnabled(false);
      }

      start(options);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void onWillTerminate(App app) {
    stop();
  }

  void start(LiveReloadOptions options) {
    if (started || !options.isEnabled()) {
      return;
    }

    lifecycle.start(options);
    started = true;
  }

  void stop() {
    if (started) {
      started = false;
      lifecycle.stop();
    }
  }

  private static Config getConfig() {
    Environment environment = Environment.getCurrent();
    return environment != null ? environment.getConfig() : null;
  }

  /**
   * Shares one reload lifecycle between the sessions of an app, until the last session or the watch
   * ends.
   *
   * @author Hyyan Abo Fakher
   * @since 26.03
   */
  static final class SharedLifecycle {

    private final LiveReloadLifecycle lifecycle;
    private int sessions;

    SharedLifecycle(LiveReloadLifecycle lifecycle) {
      this.lifecycle = lifecycle;
    }

    synchronized void start(LiveReloadOptions options) {
      if (!lifecycle.isRunning()) {
        lifecycle.start(options);
        lifecycle.openClassLoader(APP_LOADER);
      }

      sessions++;
    }

    synchronized void stop() {
      sessions--;
      if (sessions == 0) {
        lifecycle.stop();
      }
    }
  }
}
