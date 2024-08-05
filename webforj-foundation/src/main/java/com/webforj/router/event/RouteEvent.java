package com.webforj.router.event;

import com.webforj.router.NavigationContext;
import com.webforj.router.NavigationOptions;
import com.webforj.router.RoutePattern;
import com.webforj.router.Router;
import com.webforj.router.history.Location;
import com.webforj.router.history.ParametersBag;
import java.util.EventObject;
import java.util.Optional;

/**
 * {@code RouteObserverEvent} is base class for all route observer events.
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
class RouteEvent extends EventObject {
  private final transient NavigationContext context;

  /**
   * Creates a new {@code RouteObserverEvent} instance with the given {@code NavigationContext}.
   *
   * @param router the router instance
   * @param location the location instance
   * @param parameters the route parameters bag instance
   */
  protected RouteEvent(NavigationContext context) {
    super(context.getRouter());
    this.context = context;
  }

  /**
   * Returns the navigation context.
   *
   * @return the navigation context
   */
  public NavigationContext getContext() {
    return context;
  }

  /**
   * Returns the router instance.
   *
   * @return the router instance
   */
  public Router getRouter() {
    return context.getRouter();
  }

  /**
   * Returns the location instance.
   *
   * @return the location instance
   */
  public Location getLocation() {
    return context.getLocation();
  }

  /**
   * Returns the route parameters bag instance.
   *
   * @return the route parameters bag instance
   */
  public ParametersBag getRouteParameters() {
    return context.getRouteParameters();
  }

  /**
   * Returns the navigation options.
   *
   * @return the navigation options
   */
  public Optional<NavigationOptions> getOptions() {
    return context.getOptions();
  }

  /**
   * Returns the route pattern.
   *
   * @return the route pattern
   */
  public RoutePattern getRoutePattern() {
    return context.getRoutePattern();
  }
}

