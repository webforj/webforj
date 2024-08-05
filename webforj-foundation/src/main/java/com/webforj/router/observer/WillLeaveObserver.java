package com.webforj.router.observer;

import com.webforj.router.event.WillLeaveEvent;
import com.webforj.router.history.ParametersBag;
import java.io.Serializable;

/**
 * {@code WillLeaveObserver} is an interface that is used to observe the router before it attempts
 * to leave a route and detach its component from the DOM.
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 *
 * @see WillLeaveEvent
 */
@FunctionalInterface
public interface WillLeaveObserver extends Serializable {

  /**
   * This method is called before the router attempts to leave the route's component and detach it
   * from the DOM.
   *
   * <p>
   * When this method is called, the route's component is still attached to the DOM. Observers
   * should use the {@link WillLeaveEvent#accept()} method to indicate that the route's component
   * should be detached or the {@link WillLeaveEvent#reject()} method to indicate that the route
   * should be vetoed.
   * </p>
   *
   * @param event the event object
   * @param parameters the route parameters bag
   */
  void onWillLeave(WillLeaveEvent event, ParametersBag parameters);
}
