package com.webforj.router.event;

import com.webforj.router.Router;
import com.webforj.router.history.Location;
import com.webforj.router.history.ParametersBag;

/**
 * {@code WillNavigateEvent} is an event object which is fired before the router has completed the
 * rendering of the new route but before it updates the history.
 *
 * <p>
 * This event is useful to update the UI if it depends on the current route's parameters. It is
 * guaranteed that this event will be always fired event if the component is already attached to the
 * DOM.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public class WillNavigateEvent extends RouteEvent {

  /**
   * Creates a new {@code WillNavigateEvent} instance with the given {@code Router},
   * {@code Location}, and {@code ParametersBag}.
   *
   * @param router the router instance
   * @param location the location instance
   * @param parameters the route parameters bag instance
   */
  public WillNavigateEvent(Router router, Location location, ParametersBag parameters) {
    super(router, location, parameters);
  }
}
