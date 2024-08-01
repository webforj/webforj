package com.webforj.router.event;


import com.webforj.router.Location;
import com.webforj.router.ParametersBag;
import com.webforj.router.Router;
import java.util.EventObject;

/**
 * {@code RouteObserverEvent} is base class for all route observer events.
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
class RouteObserverEvent extends EventObject {
  private final transient Router router;
  private final Location location;
  private final ParametersBag routeParams;

  /**
   * Creates a new {@code RouteObserverEvent} instance with the given {@code Router},
   * {@code Location} and {@code ParametersBag}.
   *
   * @param router the router instance
   * @param location the location instance
   * @param routeParams the route parameters bag instance
   */
  protected RouteObserverEvent(Router router, Location location, ParametersBag routeParams) {
    super(router);
    this.router = router;
    this.location = location;
    this.routeParams = routeParams;
  }

  /**
   * Returns the router instance.
   *
   * @return the router instance
   */
  public Router getRouter() {
    return router;
  }

  /**
   * Returns the location instance.
   *
   * @return the location instance
   */
  public Location getLocation() {
    return location;
  }

  /**
   * Returns the route parameters bag instance.
   *
   * @return the route parameters bag instance
   */
  public ParametersBag getRouteParameters() {
    return routeParams;
  }
}

