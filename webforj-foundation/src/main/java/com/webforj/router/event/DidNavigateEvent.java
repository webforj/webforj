package com.webforj.router.event;

import com.webforj.router.Router;
import com.webforj.router.history.Location;
import com.webforj.router.history.ParametersBag;

/**
 * {@code DidNavigateEvent} is an event object which is fired after the router has successfully
 * navigated to a new route and attached its component to the DOM and updated the history.
 *
 * <p>
 * When this event is fired, the route's component is already attached to the DOM and the history is
 * updated.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public class DidNavigateEvent extends RouteEvent {

  /**
   * Creates a new {@code DidNavigateEvent} instance with the given {@code Router},
   * {@code Location}, and {@code ParametersBag}.
   *
   * @param router the router instance
   * @param location the location instance
   * @param parameters the route parameters bag instance
   */
  public DidNavigateEvent(Router router, Location location, ParametersBag parameters) {
    super(router, location, parameters);
  }
}
