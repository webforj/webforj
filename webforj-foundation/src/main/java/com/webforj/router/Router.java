package com.webforj.router;

import static com.webforj.App.console;

import com.webforj.component.Component;
import com.webforj.dispatcher.EventDispatcher;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.router.event.DidEnterEvent;
import com.webforj.router.event.DidLeaveEvent;
import com.webforj.router.event.WillEnterEvent;
import com.webforj.router.event.WillLeaveEvent;
import com.webforj.router.exception.RouteNotFoundException;
import com.webforj.router.history.History;
import com.webforj.router.history.MemoryHistory;
import com.webforj.router.history.event.HistoryStateChangeEvent;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Consumer;

/**
 * Router class responsible for navigating to a given path.
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public class Router {
  private final RouteRegistry registry;
  private final History history;
  private final RouteRenderer renderer;
  private final Map<String, RoutePattern> routesCache = new HashMap<>();
  private final EventDispatcher eventDispatcher = new EventDispatcher();
  private ListenerRegistration<HistoryStateChangeEvent> historyListener;
  private Location willNavigateToLocation;
  private RoutePattern matchedPattern;

  /**
   * Creates a new {@code Router} instance with the given {@code RouteRegistry}, {@code History} and
   * {@code ComponentNavigator}.
   *
   * @param registry the route registry
   * @param history the history object to use for history management
   * @param renderer the route RouteRenderer
   */
  public Router(RouteRegistry registry, History history, RouteRenderer renderer) {
    Objects.requireNonNull(registry, "RouteRegistry must not be null");
    Objects.requireNonNull(history, "History must not be null");
    Objects.requireNonNull(renderer, "RouteRenderer must not be null");

    this.registry = registry;
    this.history = history;
    this.renderer = renderer;
    this.renderer.addLifecycleObserver(new RouterLifecycleHandler());
    this.addHistoryStateListener();
  }

  /**
   * Creates a new {@code Router} instance with the given {@code RouteRegistry} and {@code History}.
   *
   * @param registry the route registry
   * @param history the history object to use for history management
   */
  public Router(RouteRegistry registry, History history) {
    this(registry, history, new RouteRenderer(registry));
  }

  /**
   * Creates a new {@code Router} instance with the given {@code RouteRegistry}.
   *
   * @param registry the route registry
   */
  public Router(RouteRegistry registry) {
    this(registry, new MemoryHistory());
  }

  /**
   * Navigates to the given location.
   *
   * <p>
   * This method navigates to the given location. If the location does not match any of the
   * registered routes, a {@code RouteNotFoundException} will be thrown.
   * </p>
   *
   * @param location the location to navigate to
   * @param onComplete the callback to be invoked with the result of the navigation
   */
  public void navigate(Location location, Consumer<Optional<Component>> onComplete) {
    Objects.requireNonNull(location, "Location must not be null");

    Optional<RoutePattern> routePattern = getRoutePatternForLocation(location);
    Optional<Class<? extends Component>> componentClass =
        routePattern.map(RoutePattern::getPattern).map(registry::getResolvedComponentByRoute);

    if (!routePattern.isPresent()) {
      throw new RouteNotFoundException("Failed to match route for location: " + location);
    }

    if (!componentClass.isPresent()) {
      throw new RouteNotFoundException(
          "Path matched but no component found for location: " + location);
    }

    matchedPattern = routePattern.get();
    willNavigateToLocation = location;

    // remove the history state listener to avoid loops
    removeHistoryStateListener();

    renderer.navigate(componentClass.get(), component -> {
      if (component.isPresent()) {
        console().log("Router : Navigating to: " + component.get().getClass().getSimpleName());
        history.pushState(location);
        addHistoryStateListener();
      }

      if (onComplete != null) {
        onComplete.accept(component);
      }
    });
  }

  /**
   * Navigates to the given location.
   *
   * <p>
   * This method navigates to the given location. If the location does not match any of the
   * registered routes, a {@code RouteNotFoundException} will be thrown.
   * </p>
   *
   * @param location the location to navigate to
   */
  public void navigate(Location location) {
    navigate(location, null);
  }

  /**
   * Adds a {@link WillEnterEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<WillEnterEvent> addWillEnterListener(
      EventListener<WillEnterEvent> listener) {
    return getEventDispatcher().addListener(WillEnterEvent.class, listener);
  }

  /**
   * Alias for {@link #addWillEnterListener(EventListener)}.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<WillEnterEvent> onWillEnter(EventListener<WillEnterEvent> listener) {
    return addWillEnterListener(listener);
  }

  /**
   * Adds a {@link DidEnterEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<DidEnterEvent> addDidEnterListener(
      EventListener<DidEnterEvent> listener) {
    return getEventDispatcher().addListener(DidEnterEvent.class, listener);
  }

  /**
   * Alias for {@link #addDidEnterListener(EventListener)}.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<DidEnterEvent> onDidEnter(EventListener<DidEnterEvent> listener) {
    return addDidEnterListener(listener);
  }

  /**
   * Adds a {@link WillLeaveEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<WillLeaveEvent> addWillLeaveListener(
      EventListener<WillLeaveEvent> listener) {
    return getEventDispatcher().addListener(WillLeaveEvent.class, listener);
  }

  /**
   * Alias for {@link #addWillLeaveListener(EventListener)}.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<WillLeaveEvent> onWillLeave(EventListener<WillLeaveEvent> listener) {
    return addWillLeaveListener(listener);
  }

  /**
   * Adds a {@link DidLeaveEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<DidLeaveEvent> addDidLeaveListener(
      EventListener<DidLeaveEvent> listener) {
    return getEventDispatcher().addListener(DidLeaveEvent.class, listener);
  }

  /**
   * Alias for {@link #addDidLeaveListener(EventListener)}.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<DidLeaveEvent> onDidLeave(EventListener<DidLeaveEvent> listener) {
    return addDidLeaveListener(listener);
  }

  /**
   * Retrieves the route registry.
   *
   * @return the route registry
   */
  public RouteRegistry getRegistry() {
    return registry;
  }

  /**
   * Retrieves the component navigator.
   *
   * @return the component navigator
   */
  public RouteRenderer getRenderer() {
    return renderer;
  }

  /**
   * Retrieves the history object.
   *
   * @return the history object
   */
  public History getHistory() {
    return history;
  }

  /**
   * Retrieves the event dispatcher.
   *
   * @return the event dispatcher
   */
  protected EventDispatcher getEventDispatcher() {
    return eventDispatcher;
  }

  /**
   * Gets the RoutePattern for the given location.
   *
   * @param location the location to get the RoutePattern for
   */
  protected Optional<RoutePattern> getRoutePatternForLocation(Location location) {
    RoutePattern mp = null;
    List<String> routes = registry.getResolvedRoutes();

    for (String route : routes) {
      RoutePattern pattern = routesCache.computeIfAbsent(route, RoutePattern::new);
      String currentSegment = location.getSegments().getPath();
      console().log("Router : Matching route: " + route + " with path: " + currentSegment
          + " .Does it match? " + pattern.matches(currentSegment));
      if (pattern.matches(currentSegment)) {
        mp = pattern;
        break;
      }
    }

    return Optional.ofNullable(mp);
  }

  /**
   * Adds a history state listener.
   */
  protected void addHistoryStateListener() {
    this.removeHistoryStateListener();
    historyListener = history.addHistoryStateChangeListener(
        e -> e.getLocation().ifPresent(location -> navigate(location, null)));
  }

  /**
   * Removes the history state listener.
   */
  protected void removeHistoryStateListener() {
    if (historyListener != null) {
      historyListener.remove();
    }
  }

  /**
   * RouterLifecycleHandler class responsible for handling the router lifecycle events.
   */
  private final class RouterLifecycleHandler implements RouteRendererLifecycleObserver {

    /**
     * {@inheritDoc}
     */
    @Override
    public void onRouteRendererLifecycleEvent(Component component, LifecycleEvent event,
        Consumer<Boolean> cb) {
      ParametersBag routeParams = ParametersBag
          .of(matchedPattern.extractParameters(willNavigateToLocation.getSegments().getPath()));

      switch (event) {
        case BEFORE_CREATE:
          fireWillEnterEvent(component, willNavigateToLocation, routeParams, cb);
          break;
        case AFTER_CREATE:
          fireDidEnterEvent(component, willNavigateToLocation, routeParams);
          cb.accept(true);
          break;
        case BEFORE_DESTROY:
          fireWillLeaveEvent(component, willNavigateToLocation, routeParams, cb);
          break;
        case AFTER_DESTROY:
          fireDidLeaveEvent(component, willNavigateToLocation, routeParams);
          cb.accept(true);
          break;
        default:
          cb.accept(true);
          break;
      }
    }

    /**
     * Fires the {@link WillEnterEvent} event.
     *
     * @param component the component
     * @param location the location
     * @param routeParams the route parameters bag
     * @param cb the callback to indicate completion
     */
    void fireWillEnterEvent(Component component, Location location, ParametersBag routeParams,
        Consumer<Boolean> cb) {
      WillEnterEvent event = new WillEnterEvent(Router.this, location, routeParams, cb);
      getEventDispatcher().dispatchEvent(event);

      if (component instanceof WillEnterObserver willEnterObserver) {
        willEnterObserver.onWillEnterRoute(event, routeParams);
      } else {
        cb.accept(true);
      }
    }

    /**
     * Fires the {@link DidEnterEvent} event.
     *
     * @param component the component
     * @param location the location
     * @param routeParams the route parameters bag
     */
    void fireDidEnterEvent(Component component, Location location, ParametersBag routeParams) {
      DidEnterEvent event = new DidEnterEvent(Router.this, location, routeParams);
      getEventDispatcher().dispatchEvent(event);

      if (component instanceof DidEnterObserver didEnterObserver) {
        didEnterObserver.onDidEnterRoute(event, routeParams);
      }
    }

    /**
     * Fires the {@link WillLeaveEvent} event.
     *
     * @param component the component
     * @param location the location
     * @param routeParams the route parameters bag
     * @param cb the callback to indicate completion
     */
    void fireWillLeaveEvent(Component component, Location location, ParametersBag routeParams,
        Consumer<Boolean> cb) {
      WillLeaveEvent event = new WillLeaveEvent(Router.this, location, routeParams, cb);
      getEventDispatcher().dispatchEvent(event);

      if (component instanceof WillLeaveObserver willLeaveObserver) {
        willLeaveObserver.onWillLeaveRoute(event, routeParams);
      } else {
        cb.accept(true);
      }
    }

    /**
     * Fires the {@link DidLeaveEvent} event.
     *
     * @param component the component
     * @param location the location
     * @param routeParams the route parameters bag
     */
    void fireDidLeaveEvent(Component component, Location location, ParametersBag routeParams) {
      DidLeaveEvent event = new DidLeaveEvent(Router.this, location, routeParams);
      getEventDispatcher().dispatchEvent(event);

      if (component instanceof DidLeaveObserver didLeaveObserver) {
        didLeaveObserver.onDidLeaveRoute(event, routeParams);
      }
    }
  }
}
