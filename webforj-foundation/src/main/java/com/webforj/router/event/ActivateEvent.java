package com.webforj.router.event;

import com.webforj.router.NavigationContext;

/**
 * {@code ActivateEvent} is an event object which is fired when the router activates (reuses) a
 * cached component instead of creating a new one.
 *
 * @author Hyyan Abo Fakher
 * @since 25.03
 *
 * @see DidEnterEvent
 */
public class ActivateEvent extends RouteEvent {

  /**
   * Creates a new {@code ActivateEvent} instance with the given {@code NavigationContext}.
   *
   * @param context the navigation context
   */
  public ActivateEvent(NavigationContext context) {
    super(context);
  }
}
