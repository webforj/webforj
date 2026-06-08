package com.webforj.devtools.livereload;

import jakarta.servlet.ServletContainerInitializer;
import jakarta.servlet.ServletContext;
import java.util.Set;

/**
 * Registers the live reload context listener on every servlet container start.
 *
 * <p>
 * A servlet container discovers this initializer through the service loader and runs it on
 * deployment even when the application descriptor is marked metadata complete, which turns off web
 * fragment scanning. Registering the context listener here therefore brings the live reload up on a
 * standalone container, such as the development Jetty, whether or not it scans fragments. On Spring
 * the live reload runs from a context bean instead, since the embedded container does not run this
 * initializer.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public class LiveReloadServletContainerInitializer implements ServletContainerInitializer {

  /**
   * {@inheritDoc}
   */
  @Override
  public void onStartup(Set<Class<?>> classes, ServletContext context) {
    context.addListener(new WatchContextListener());
  }
}
