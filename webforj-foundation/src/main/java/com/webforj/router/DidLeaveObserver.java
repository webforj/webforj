package com.webforj.router;

import com.webforj.router.event.DidLeaveEvent;
import java.io.Serializable;

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
public interface DidLeaveObserver extends Serializable {

  /**
   * This method is called after the router leaves the route's component and detaches it from the
   * DOM.
   *
   * @param event the event object
   * @param parameters the route parameters bag
   */
  void onDidLeaveRoute(DidLeaveEvent event, ParametersBag parameters);
}
