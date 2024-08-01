package com.webforj.router.event;

import com.webforj.router.Location;
import com.webforj.router.ParametersBag;
import com.webforj.router.Router;

/**
 * {@code WillLeaveEvent} is an event object which is fired before the router leaves a route.
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public class WillLeaveEvent extends RouteObserverEvent {

  /**
   * Creates a new {@code WillLeaveEvent} instance with the given {@code Router}, {@code Location}
   * and {@code ParametersBag}.
   *
   * @param router the router instance
   * @param location the location instance
   * @param routeParams the route parameters bag instance
   */
  public WillLeaveEvent(Router router, Location location, ParametersBag routeParams) {
    super(router, location, routeParams);
  }
}
