package com.webforj.router.observer;

import com.webforj.router.event.DidNavigateEvent;
import com.webforj.router.history.ParametersBag;
import java.io.Serializable;

/**
 * {@code DidNavigateObserver} is an interface that is used to observe the router after it has
 * successfully navigated to a new route, attached its component to the DOM, and updated the
 * history.
 *
 * <p>
 * When this method is called, the route's component is already attached to the DOM and the history
 * is updated.
 * </p>
 *
 * @since 24.11
 *
 * @see DidNavigateEvent
 */
@FunctionalInterface
public interface DidNavigateObserver extends Serializable {

  /**
   * This method is called after the router has successfully navigated to a new route, attached its
   * component to the DOM, and updated the history.
   *
   * @param event the event object
   * @param parameters the route parameters bag
   */
  void onDidNavigate(DidNavigateEvent event, ParametersBag parameters);
}
