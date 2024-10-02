package com.webforj.router.event;

import com.webforj.router.NavigationContext;
import com.webforj.router.NavigationOptions;
import com.webforj.router.Router;
import com.webforj.router.history.History;
import com.webforj.router.history.Location;
import java.util.EventObject;
import java.util.Optional;

/**
 * {@code RouteObserverEvent} is base class for all route observer events.
 *
 * @author Hyyan Abo Fakher
 * @since 24.12
 */
class RouteEvent extends EventObject {
  private final transient NavigationContext context;

  /**
   * Creates a new {@code RouteObserverEvent} instance with the given {@code NavigationContext}.
   *
   * @param context the navigation context
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
   * Returns the history instance.
   *
   * @return the history instance
   */
  public History getHistory() {
    return getRouter().getHistory();
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
   * Returns the state at the top of the history stack.
   *
   * @param <T> the type of the state object
   * @return an {@link Optional} containing the state object if it is present, otherwise an empty
   *         {@code Optional}
   *
   * @see History#getState(Class)
   */
  public <T> Optional<T> getState(Class<T> classOfT) {
    return getHistory().getState(classOfT);
  }

  /**
   * Returns the state at the top of the history stack.
   *
   * @return an {@link Optional} containing the state object if it is present, otherwise an empty
   *         {@code Optional}
   *
   * @see History#getState()
   */
  public Optional<Object> getState() {
    return getHistory().getState();
  }

  /**
   * Returns the navigation options.
   *
   * @return the navigation options
   */
  public Optional<NavigationOptions> getOptions() {
    return context.getOptions();
  }
}

