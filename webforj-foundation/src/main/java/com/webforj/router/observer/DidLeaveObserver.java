package com.webforj.router.observer;

import com.webforj.router.event.DidLeaveEvent;
import com.webforj.router.event.WillLeaveEvent;
import com.webforj.router.history.ParametersBag;

/**
 * {@code DidLeaveObserver} is an interface that is used to observe the router after it leaves a
 * route and detaches its component from the DOM.
 *
 * <p>
 * When this method is called, the route's component is already detached from the DOM.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 *
 * @see WillLeaveEvent
 */
@FunctionalInterface
public interface DidLeaveObserver {

  /**
   * This method is called after the router leaves the route's component and detaches it from the
   * DOM.
   *
   * @param event the event object
   * @param parameters the route parameters bag
   */
  void onDidLeave(DidLeaveEvent event, ParametersBag parameters);
}
