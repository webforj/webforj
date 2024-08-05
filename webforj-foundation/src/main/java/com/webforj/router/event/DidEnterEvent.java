package com.webforj.router.event;

import com.webforj.router.NavigationContext;

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
   * Creates a new {@code DidEnterEvent} instance with the given {@code NavigationContext}.
   *
   * @param context the navigation context
   */
  public DidEnterEvent(NavigationContext context) {
    super(context);
  }
}
