package com.webforj.router;

import com.webforj.router.event.WillEnterEvent;
import java.io.Serializable;

/**
 * {@code WillEnterObserver} is an interface that is used to observe the router before it renders a
 * route's component.
 *
 * @since 24.11
 */
@FunctionalInterface
public interface WillEnterObserver extends Serializable {

  /**
   * This method is called before the router renders the route's component and attaches it to the
   * DOM.
   *
   * <p>
   * Observers should use the {@link WillEnterEvent#accept()} method to indicate that the route's
   * component should be rendered or the {@link WillEnterEvent#reject()} method to indicate that the
   * route should be vetoed.
   * </p>
   *
   * @param event the event object
   * @param parameters the route parameters bag
   */
  void onWillEnterRoute(WillEnterEvent event, ParametersBag parameters);
}
