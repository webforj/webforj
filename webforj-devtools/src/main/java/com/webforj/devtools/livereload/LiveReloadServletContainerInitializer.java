package com.webforj.devtools.livereload;

import jakarta.servlet.DispatcherType;
import jakarta.servlet.FilterRegistration;
import jakarta.servlet.ServletContainerInitializer;
import jakarta.servlet.ServletContext;
import java.util.EnumSet;
import java.util.Set;

/**
 * Registers the live reload context listener on every servlet container start.
 *
 * <p>
 * A servlet container discovers this initializer through the service loader and runs it on
 * deployment even when the application descriptor is marked metadata complete, which turns off web
 * fragment scanning. Registering the context listener and the restart notice filter here therefore
 * brings the live reload up on a standalone container, such as the development Jetty, whether or
 * not it scans fragments. On Spring the live reload runs from a context bean instead, since the
 * embedded container does not run this initializer.
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
    LiveReloadLifecycle lifecycle = new LiveReloadLifecycle();
    context.addListener(new WatchContextListener(lifecycle));

    // The container destroys the filters ahead of the servlets on a redeploy, so this filter
    // sends the restart notice before the sessions are torn down. The registration is null when
    // the context already holds a filter under this name, the notice then comes from the existing
    // registration and the context listener fallback.
    FilterRegistration.Dynamic registration = context
        .addFilter(LiveReloadRestartFilter.class.getName(), new LiveReloadRestartFilter(lifecycle));
    if (registration != null) {
      registration.addMappingForUrlPatterns(EnumSet.of(DispatcherType.REQUEST), false, "/*");
    }
  }
}
