package com.webforj.router;

import com.webforj.component.Component;
import com.webforj.dispatcher.EventDispatcher;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.router.event.DidEnterEvent;
import com.webforj.router.event.DidLeaveEvent;
import com.webforj.router.event.DidNavigateEvent;
import com.webforj.router.event.WillEnterEvent;
import com.webforj.router.event.WillLeaveEvent;
import com.webforj.router.event.WillNavigateEvent;
import com.webforj.router.exception.RouteNotFoundException;
import com.webforj.router.history.History;
import com.webforj.router.history.Location;
import com.webforj.router.history.MemoryHistory;
import com.webforj.router.history.ParametersBag;
import com.webforj.router.history.event.HistoryStateChangeEvent;
import com.webforj.router.observer.DidNavigateObserver;
import com.webforj.router.observer.WillNavigateObserver;
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
    this.renderer.addObserver(new RouteRendererDispatcher(getEventDispatcher()));
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
   * @param options the navigate options
   * @param onComplete the callback to be invoked with the result of the navigation
   */
  public void navigate(Location location, NavigationOptions options,
      Consumer<Component> onComplete) {
    Objects.requireNonNull(location, "Location must not be null");

    Optional<RoutePattern> routePattern = getRoutePatternForLocation(location);
    Optional<Class<? extends Component>> componentClass = routePattern.map(RoutePattern::getPattern)
        .map(c -> registry.getComponentByRoute(c).orElse(null));

    if (!routePattern.isPresent()) {
      throw new RouteNotFoundException("Failed to match route for location: " + location);
    }

    if (!componentClass.isPresent()) {
      throw new RouteNotFoundException(
          "Path matched but no component found for location: " + location);
    }

    RoutePattern matchedPattern = routePattern.get();
    NavigationContext context = new NavigationContext(this, location, options, matchedPattern,
        ParametersBag.of(matchedPattern.extractParameters(location.getSegments().getPath())));

    // remove the history state listener to avoid loops
    removeHistoryStateListener();

    renderer.render(componentClass.get(), context, component -> {
      handleNavigateComplete(component, context);

      if (onComplete != null) {
        onComplete.accept(component);
      }

      addHistoryStateListener();
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
   * @param options the navigate options
   */
  public void navigate(Location location, NavigationOptions options) {
    navigate(location, options, null);
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
  public void navigate(Location location, Consumer<Component> onComplete) {
    navigate(location, new NavigationOptions(), onComplete);
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
    navigate(location, new NavigationOptions(), null);
  }

  /**
   * Navigates to the location corresponding to the given component.
   *
   * <p>
   * This method navigates to the location corresponding to the given component. If no route matches
   * the component, a {@code RouteNotFoundException} will be thrown.
   * </p>
   *
   * @param component the component class to navigate to
   * @param options the navigate options
   * @param routeParameters a map of parameters to be included in the URL
   * @param onComplete the callback to be invoked with the result of the navigation
   */
  public void navigate(Class<? extends Component> component, NavigationOptions options,
      Map<String, String> routeParameters, Consumer<Component> onComplete) {
    Objects.requireNonNull(component, "Component class must not be null");

    Optional<String> route = registry.getRouteByComponent(component);
    if (!route.isPresent()) {
      throw new RouteNotFoundException(
          "No route found for component: " + component.getSimpleName());
    }

    RoutePattern pattern = routesCache.computeIfAbsent(route.get(), RoutePattern::new);
    String path = pattern.buildUrl(routeParameters != null ? routeParameters : Map.of());
    Location location = new Location(path);

    navigate(location, options, onComplete);
  }

  /**
   * Navigates to the location corresponding to the given component with options and parameters.
   *
   * @param component the component class to navigate to
   * @param options the navigate options
   * @param params a map of parameters to be included in the URL
   */
  public void navigate(Class<? extends Component> component, NavigationOptions options,
      Map<String, String> params) {
    navigate(component, options, params, null);
  }

  /**
   * Navigates to the location corresponding to the given component with parameters and a completion
   * callback.
   *
   * @param component the component class to navigate to
   * @param params a map of parameters to be included in the URL
   * @param onComplete the callback to be invoked with the result of the navigation
   */
  public void navigate(Class<? extends Component> component, Map<String, String> params,
      Consumer<Component> onComplete) {
    navigate(component, new NavigationOptions(), params, onComplete);
  }

  /**
   * Navigates to the location corresponding to the given component with parameters.
   *
   * @param component the component class to navigate to
   * @param params a map of parameters to be included in the URL
   */
  public void navigate(Class<? extends Component> component, Map<String, String> params) {
    navigate(component, new NavigationOptions(), params, null);
  }

  /**
   * Navigates to the location corresponding to the given component with options and a completion
   * callback.
   *
   * @param component the component class to navigate to
   * @param options the navigate options
   * @param onComplete the callback to be invoked with the result of the navigation
   */
  public void navigate(Class<? extends Component> component, NavigationOptions options,
      Consumer<Component> onComplete) {
    navigate(component, options, null, onComplete);
  }

  /**
   * Navigates to the location corresponding to the given component with options.
   *
   * @param component the component class to navigate to
   * @param options the navigate options
   */
  public void navigate(Class<? extends Component> component, NavigationOptions options) {
    navigate(component, options, null, null);
  }

  /**
   * Navigates to the location corresponding to the given component with a completion callback.
   *
   * @param component the component class to navigate to
   * @param onComplete the callback to be invoked with the result of the navigation
   */
  public void navigate(Class<? extends Component> component, Consumer<Component> onComplete) {
    navigate(component, new NavigationOptions(), null, onComplete);
  }

  /**
   * Navigates to the location corresponding to the given component.
   *
   * @param component the component class to navigate to
   */
  public void navigate(Class<? extends Component> component) {
    navigate(component, new NavigationOptions(), null, null);
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
   * Adds a {@link WillNavigateEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<WillNavigateEvent> addWillNavigateListener(
      EventListener<WillNavigateEvent> listener) {
    return getEventDispatcher().addListener(WillNavigateEvent.class, listener);
  }

  /**
   * Alias for {@link #addWillNavigateListener(EventListener)}.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<WillNavigateEvent> onWillNavigate(
      EventListener<WillNavigateEvent> listener) {
    return addWillNavigateListener(listener);
  }

  /**
   * Adds a {@link DidNavigateEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<DidNavigateEvent> addDidNavigateListener(
      EventListener<DidNavigateEvent> listener) {
    return getEventDispatcher().addListener(DidNavigateEvent.class, listener);
  }

  /**
   * Alias for {@link #addDidNavigateListener(EventListener)}.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<DidNavigateEvent> onDidNavigate(
      EventListener<DidNavigateEvent> listener) {
    return addDidNavigateListener(listener);
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
    List<String> routes = registry.getAvailableRoutes();

    for (String route : routes) {
      RoutePattern pattern = routesCache.computeIfAbsent(route, RoutePattern::new);
      String currentSegment = location.getSegments().getPath();
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
        e -> e.getLocation().ifPresent(location -> navigate(location)));
  }

  /**
   * Removes the history state listener.
   */
  protected void removeHistoryStateListener() {
    if (historyListener != null) {
      historyListener.remove();
    }
  }

  private void handleNavigateComplete(Component component, NavigationContext context) {
    ParametersBag routeParams = context.getRouteParameters();
    Location willNavigateToLocation = context.getLocation();
    Optional<NavigationOptions> willNavigateOptions = context.getOptions();

    WillNavigateEvent event = new WillNavigateEvent(context);
    willNavigateOptions.ifPresent(options -> {
      if (options.isFireEvents()) {
        getEventDispatcher().dispatchEvent(event);
      }

      if (options.isInvokeObservers()
          && component instanceof WillNavigateObserver willNavigateObserver) {
        willNavigateObserver.onWillNavigate(event, routeParams);
      }

      if (options.isUpdateHistory()) {
        NavigationOptions.NavigationType type = options.getNavigationType();
        Object state = options.getState();
        switch (type) {
          case PUSH:
            history.pushState(state, willNavigateToLocation);
            break;
          case REPLACE:
            history.replaceState(state, willNavigateToLocation);
            break;
          default:
            break;
        }
      }

      DidNavigateEvent didNavigateEvent = new DidNavigateEvent(context);

      if (options.isFireEvents()) {
        getEventDispatcher().dispatchEvent(didNavigateEvent);
      }

      if (options.isInvokeObservers()
          && component instanceof DidNavigateObserver didNavigateObserver) {
        didNavigateObserver.onDidNavigate(didNavigateEvent, routeParams);
      }
    });
  }
}
