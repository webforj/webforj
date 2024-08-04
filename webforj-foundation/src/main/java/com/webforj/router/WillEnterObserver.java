package com.webforj.router;

import com.webforj.router.event.WillEnterEvent;
import java.io.Serializable;

/**
 * {@code WillEnterObserver} is an interface that is used to observe the router before it attempts
 * to enter a route and attach its component to the DOM.
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 *
 * @see WillEnterEvent
 */
@FunctionalInterface
public interface WillEnterObserver extends Serializable {

  /**
   * This method is called before the router attempts to enter the route's component and attach it
   * to the DOM.
   *
   * <p>
   * When this method is called, the route's component is initialized in memory but not attached to
   * the DOM yet. Observers should use the {@link WillEnterEvent#accept()} method to indicate that
   * the route's component should be rendered or the {@link WillEnterEvent#reject()} method to
   * indicate that the route should be vetoed.
   * </p>
   *
   * @param event the event object
   * @param parameters the route parameters bag
   */
  void onWillEnterRoute(WillEnterEvent event, ParametersBag parameters);
}
