package com.webforj.router;

import com.webforj.router.history.Location;
import com.webforj.router.history.ParametersBag;

/**
 * The navigation context.
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public class NavigationContext {
  private final Router router;
  private final Location location;
  private final NavigationOptions options;
  private final RoutePattern routePattern;
  private final ParametersBag routeParameters;

  /**
   * Creates a new navigation context.
   *
   * @param router the router
   * @param location the location
   * @param options the navigation options
   * @param routePattern the route pattern
   * @param routeParameters the route parameters
   */
  public NavigationContext(Router router, Location location, NavigationOptions options,
      RoutePattern routePattern, ParametersBag routeParameters) {
    this.router = router;
    this.location = location;
    this.options = options;
    this.routePattern = routePattern;
    this.routeParameters = routeParameters;
  }

  /**
   * Gets the router.
   *
   * @return the router
   */
  public Router getRouter() {
    return router;
  }

  /**
   * Gets the location.
   *
   * @return the location
   */
  public Location getLocation() {
    return location;
  }

  /**
   * Gets the navigation options.
   *
   * @return the navigation options
   */
  public NavigationOptions getOptions() {
    return options;
  }

  /**
   * Gets the route pattern.
   *
   * @return the route pattern
   */
  public RoutePattern getRoutePattern() {
    return routePattern;
  }

  /**
   * Gets the route parameters.
   *
   * @return the route parameters
   */
  public ParametersBag getRouteParameters() {
    return routeParameters;
  }
}
