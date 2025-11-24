package com.webforj.router;

import java.util.ServiceLoader;

/**
 * Service Provider Interface for providing custom {@link RouteRegistry} implementations.
 *
 * <p>
 * This SPI allows integration frameworks to provide pre-populated {@link RouteRegistry} instances
 * using their own classpath scanning mechanisms instead of relying on the default scanning. This is
 * useful when the default scanner cannot find classes in certain environments or JAR structures.
 * </p>
 *
 * <p>
 * Implementations should be registered using Java's {@link ServiceLoader} mechanism by creating a
 * file {@code META-INF/services/com.webforj.router.RouteRegistryProvider} containing the fully
 * qualified class name of the provider implementation. When multiple providers are available, the
 * first provider found by the {@link ServiceLoader} will be used.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.11
 */
public interface RouteRegistryProvider {

  /**
   * Creates and returns a {@link RouteRegistry} populated with discovered routes.
   *
   * <p>
   * This method is called by the webforJ framework during router initialization. The implementation
   * should scan for and register all route-annotated classes using whatever classpath scanning
   * mechanism is appropriate for the provider.
   * </p>
   *
   * @param packages the packages to scan for routes
   * @return a populated {@link RouteRegistry}, never {@code null}.
   */
  RouteRegistry createRouteRegistry(String[] packages);
}
