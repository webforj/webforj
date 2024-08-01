package com.webforj.router;

import com.webforj.PendingResult;
import com.webforj.router.event.WillLeaveEvent;
import java.io.Serializable;

/**
 * {@code WillLeaveObserver} is an interface that is used to observe the router before it a destroys
 * a route's component.
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
@FunctionalInterface
public interface WillLeaveObserver extends Serializable {

  /**
   * This method is called before the router destroys the route's component and detach it from the
   * DOM.
   *
   * <p>
   * If the observer returns a {@code PendingResult} which resolves to {@code false}, the router
   * will not destroy the route's component and will not detach it from the DOM and the routing
   * process for child routes will be stopped.
   * </p>
   *
   * @param event the event object
   * @param routeParams the route parameters bag
   *
   * @return a PendingResult that resolves to a boolean value
   */
  PendingResult<Boolean> onWillLeaveRoute(WillLeaveEvent event, ParametersBag routeParams);
}
