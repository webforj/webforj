package com.webforj.router;

import com.webforj.router.event.DidEnterEvent;
import java.io.Serializable;

/**
 * {@code DidEnterObserver} is an interface that is used to observe the router after a route's
 * component is rendered.
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
@FunctionalInterface
public interface DidEnterObserver extends Serializable {

  /**
   * This method is called after the router renders the route's component and attach it to the DOM.
   *
   * @param event the event object
   * @param routeParams the route parameters bag
   */
  void onDidEnterRoute(DidEnterEvent event, ParametersBag routeParams);
}
