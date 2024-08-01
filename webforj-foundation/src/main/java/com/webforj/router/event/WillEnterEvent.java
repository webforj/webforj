package com.webforj.router.event;

import com.webforj.router.Location;
import com.webforj.router.ParametersBag;
import com.webforj.router.Router;

/**
 * {@code WillEnterEvent} is an event object which is fired before the router enters a route.
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public class WillEnterEvent extends RouteObserverEvent {

  /**
   * Creates a new {@code WillEnterEvent} instance with the given {@code Router}, {@code Location}
   * and {@code ParametersBag}.
   *
   * @param router the router instance
   * @param location the location instance
   * @param routeParams the route parameters bag instance
   */
  public WillEnterEvent(Router router, Location location, ParametersBag routeParams) {
    super(router, location, routeParams);
  }
}
