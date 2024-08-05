package com.webforj.router.event;

import com.webforj.router.NavigationContext;

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
   * Creates a new {@code DidNavigateEvent} instance with the given {@code NavigationContext}.
   *
   * @param context the navigation context
   */
  public DidNavigateEvent(NavigationContext context) {
    super(context);
  }
}
