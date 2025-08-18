package com.webforj.router.observer;

import com.webforj.router.event.ActivateEvent;
import com.webforj.router.history.ParametersBag;

/**
 * {@code ActivateObserver} is an interface that is used to observe when a cached route component is
 * activated (reused) by the router.
 *
 * @author Hyyan Abo Fakher
 * @since 25.03
 *
 * @see ActivateEvent
 */
@FunctionalInterface
public interface ActivateObserver {

  /**
   * This method is called when a cached route component is activated (reused).
   *
   * @param event the event object
   * @param parameters the route parameters bag
   */
  void onActivate(ActivateEvent event, ParametersBag parameters);
}
