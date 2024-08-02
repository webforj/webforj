package com.webforj.router;

import com.webforj.router.event.WillLeaveEvent;
import java.io.Serializable;

/**
 * {@code WillLeaveObserver} is an interface that is used to observe the router before it destroys a
 * route's component.
 *
 * @since 24.11
 */
@FunctionalInterface
public interface WillLeaveObserver extends Serializable {

  /**
   * This method is called before the router destroys the route's component and detaches it from the
   * DOM.
   *
   * <p>
   * Observers should use the {@link WillLeaveEvent#accept(boolean)} method to indicate that the
   * route's component should be destroyed or the {@link WillLeaveEvent#reject()} method to indicate
   * that the route should be vetoed.
   * </p>
   *
   * @param event the event object
   * @param parameters the route parameters bag
   */
  void onWillLeaveRoute(WillLeaveEvent event, ParametersBag parameters);
}
