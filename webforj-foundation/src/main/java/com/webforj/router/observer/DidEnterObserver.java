package com.webforj.router.observer;

import com.webforj.router.event.DidEnterEvent;
import com.webforj.router.history.ParametersBag;

/**
 * {@code DidEnterRouteObserver} is an interface that is used to observe the router after it enters
 * a route and attaches its component to the DOM.
 *
 * <p>
 * When this method is called, the route's component is already attached to the DOM.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.12
 *
 * @see WillEnterEvent
 */
@FunctionalInterface
public interface DidEnterObserver {

  /**
   * This method is called after the router enters the route's component and attaches it to the DOM.
   *
   * @param event the event object
   * @param parameters the route parameters bag
   */
  void onDidEnter(DidEnterEvent event, ParametersBag parameters);
}
