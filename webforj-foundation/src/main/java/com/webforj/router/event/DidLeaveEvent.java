package com.webforj.router.event;

import com.webforj.router.NavigationContext;

/**
 * {@code DidLeaveEvent} is an event object which is fired after the router leaves a route and
 * detaches its component from the DOM.
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 *
 * @see WillLeaveEvent
 */
public class DidLeaveEvent extends RouteEvent {

  /**
   * Creates a new {@code DidLeaveEvent} instance with the given {@code NavigationContext}.
   *
   * @param context the navigation context
   */
  public DidLeaveEvent(NavigationContext context) {
    super(context);
  }
}
