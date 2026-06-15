package com.webforj.devtools.livereload;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;
import jakarta.servlet.ServletContextEvent;
import jakarta.servlet.ServletContextListener;

/**
 * Owns the live reload across a servlet context, starting it on deployment and stopping it on
 * teardown.
 *
 * <p>
 * The {@link LiveReloadServletContainerInitializer} registers this listener on a standalone servlet
 * container, such as the development Jetty, so the container invokes it on every deployment. It
 * reads the webforJ configuration, brings the reload server and the receiver up when live reload is
 * on, and tears them down on the next redeploy, so a deployment binds the reload port cleanly and
 * runs a single receiver. On Spring the same lifecycle runs from a context bean instead.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public class WatchContextListener implements ServletContextListener {

  private final LiveReloadLifecycle lifecycle = new LiveReloadLifecycle();

  /**
   * {@inheritDoc}
   */
  @Override
  public void contextInitialized(ServletContextEvent event) {
    lifecycle.start(LiveReloadOptions.from(loadConfig()));
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void contextDestroyed(ServletContextEvent event) {
    lifecycle.stop();
  }

  private static Config loadConfig() {
    ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
    if (classLoader == null) {
      classLoader = WatchContextListener.class.getClassLoader();
    }

    return ConfigFactory.parseResourcesAnySyntax(classLoader, "webforj.conf");
  }
}
