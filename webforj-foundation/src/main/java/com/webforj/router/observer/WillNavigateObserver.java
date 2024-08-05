package com.webforj.router.observer;

import com.webforj.router.event.WillNavigateEvent;
import com.webforj.router.history.ParametersBag;
import java.io.Serializable;

/**
 * {@code WillNavigateObserver} is an interface that is used to observe the router before it updates
 * the history.
 *
 * <p>
 * This event is fired before the router has completed the rendering of the new route but before it
 * updates the history. It is useful to update the UI if it depends on the current route's
 * parameters. It is guaranteed that this event will always be fired even if the component is
 * already attached to the DOM.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 *
 * @see WillNavigateEvent
 */
@FunctionalInterface
public interface WillNavigateObserver extends Serializable {

  /**
   * This method is called before the router updates the history.
   *
   * @param event the event object
   * @param parameters the route parameters bag
   */
  void onWillNavigate(WillNavigateEvent event, ParametersBag parameters);
}
