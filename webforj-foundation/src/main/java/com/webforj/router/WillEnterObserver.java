package com.webforj.router;

import com.webforj.PendingResult;
import com.webforj.router.event.WillEnterEvent;
import java.io.Serializable;

/**
 * {@code WillEnterObserver} is an interface that is used to observe the router before it a renders
 * a route's component.
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
@FunctionalInterface
public interface WillEnterObserver extends Serializable {

  /**
   * This method is called before the router router renders the route's component and attach it to
   * the DOM.
   *
   * <p>
   * If the observer returns a {@code PendingResult} which resolves to {@code false}, the router
   * will not render the route's component and will not attach it to the DOM and the routing process
   * for child routes will be stopped.
   * </p>
   *
   * @param event the event object
   * @param routeParams the route parameters bag
   *
   * @return a PendingResult that resolves to a boolean value
   */
  PendingResult<Boolean> onWillEnterRoute(WillEnterEvent event, ParametersBag routeParams);
}
