package com.webforj.router.event;

import com.webforj.router.Location;
import com.webforj.router.ParametersBag;
import com.webforj.router.Router;
import java.util.function.Consumer;

/**
 * {@code WillEnterEvent} is an event object which is fired before the router enters a route.
 *
 * <p>
 * Observers can use the {@link #accept()} method to allow the route to proceed or the
 * {@link #reject()} method to veto the route.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public class WillEnterEvent extends RouteObserverEvent {

  private final transient Consumer<Boolean> allowEnter;

  /**
   * Creates a new {@code WillEnterEvent} instance with the given {@code Router}, {@code Location},
   * {@code ParametersBag}, and {@code Consumer<Boolean>}.
   *
   * @param router the router instance
   * @param location the location instance
   * @param parameters the route parameters bag instance
   * @param allowEnter the callback consumer to signal whether the route should be allowed
   */
  public WillEnterEvent(Router router, Location location, ParametersBag parameters,
      Consumer<Boolean> allowEnter) {
    super(router, location, parameters);
    this.allowEnter = allowEnter;
  }

  /**
   * Signals that whether the route should be allowed to proceed.
   *
   * @param value {@code true} to allow the route to proceed, {@code false} to veto the route
   */
  public void accept(boolean value) {
    allowEnter.accept(value);
  }
}
