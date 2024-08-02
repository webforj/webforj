package com.webforj.router;

import com.webforj.router.event.DidLeaveEvent;
import java.io.Serializable;

/**
 * {@code DidLeaveObserver} is an interface that is used to observe the router after a route's
 * component is destroyed.
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
@FunctionalInterface
public interface DidLeaveObserver extends Serializable {

  /**
   * This method is called after the router destroys the route's component and detach it from the
   * DOM.
   *
   * @param event the event object
   * @param parameters the route parameters bag
   */
  void onDidLeaveRoute(DidLeaveEvent event, ParametersBag parameters);
}
