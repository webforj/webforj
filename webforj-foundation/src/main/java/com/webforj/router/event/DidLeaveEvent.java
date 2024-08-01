package com.webforj.router.event;

import com.webforj.router.Location;
import com.webforj.router.ParametersBag;
import com.webforj.router.Router;

/**
 * {@code DidLeaveEvent} is an event object which is fired after the router leaves a route.
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public class DidLeaveEvent extends RouteObserverEvent {

  /**
   * Creates a new {@code DidLeaveEvent} instance with the given {@code Router}, {@code Location}
   * and {@code ParametersBag}.
   *
   * @param router the router instance
   * @param location the location instance
   * @param routeParams the route parameters bag instance
   */
  public DidLeaveEvent(Router router, Location location, ParametersBag routeParams) {
    super(router, location, routeParams);
  }
}
