package com.webforj.router.event;

import com.webforj.router.NavigationContext;

/**
 * {@code NavigateEvent} is an event object which is fired after the router has successfully
 * navigated to a new route and attached its component to the DOM and updated the history.
 *
 * <p>
 * When this event is fired, the route's component is already attached to the DOM and the history is
 * updated.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.12
 */
public class NavigateEvent extends RouteEvent {

  /**
   * Creates a new {@code NavigateEvent} instance with the given {@code NavigationContext}.
   *
   * @param context the navigation context
   */
  public NavigateEvent(NavigationContext context) {
    super(context);
  }
}
