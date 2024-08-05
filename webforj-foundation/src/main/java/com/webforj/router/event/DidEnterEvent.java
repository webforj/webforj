package com.webforj.router.event;

import com.webforj.router.Router;
import com.webforj.router.history.Location;
import com.webforj.router.history.ParametersBag;

/**
 * {@code DidEnterEvent} is an event object which is fired after the router enters a route and
 * attaches its component to the DOM.
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 *
 * @see WillEnterEvent
 */
public class DidEnterEvent extends RouteEvent {

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
