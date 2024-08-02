package com.webforj.router.event;

import com.webforj.router.Location;
import com.webforj.router.ParametersBag;
import com.webforj.router.Router;

/**
 * {@code DidEnterEvent} is an event object which is fired after the router enters a route.
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public class DidEnterEvent extends RouteObserverEvent {

  /**
   * Creates a new {@code DidEnterEvent} instance with the given {@code Router}, {@code Location}
   * and {@code ParametersBag}.
   *
   * @param router the router instance
   * @param location the location instance
   * @param parameters the route parameters bag instance
   */
  public DidEnterEvent(Router router, Location location, ParametersBag parameters) {
    super(router, location, parameters);
  }
}
