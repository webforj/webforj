package com.webforj.router.event;

import com.webforj.router.Location;
import com.webforj.router.ParametersBag;
import com.webforj.router.Router;

/**
 * {@code DidLeaveEvent} is an event object which is fired after the router leaves a route and
 * detaches its component from the DOM.
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 *
 * @see WillLeaveEvent
 */
public class DidLeaveEvent extends RouteObserverEvent {

  /**
   * Creates a new {@code DidLeaveEvent} instance with the given {@code Router}, {@code Location}
   * and {@code ParametersBag}.
   *
   * @param router the router instance
   * @param location the location instance
   * @param parameters the route parameters bag instance
   */
  public DidLeaveEvent(Router router, Location location, ParametersBag parameters) {
    super(router, location, parameters);
  }
}
