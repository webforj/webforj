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
import com.webforj.router.history.BrowserHistory;
import com.webforj.router.history.History;
import com.webforj.router.history.Location;
import com.webforj.router.history.ParametersBag;
import com.webforj.router.history.SegmentsBag;
import com.webforj.router.history.event.HistoryStateChangeEvent;
import com.webforj.router.observer.DidNavigateObserver;
import com.webforj.router.observer.WillNavigateObserver;
import static com.webforj.App.console;
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
  private String root;

  /**
   * Creates a new {@code Router} instance.
   *
   * @param root the base path to use for all routes
   * @param registry the route registry
   * @param history the history object to use for history management
   * @param renderer the route RouteRenderer
   */
  public Router(String root, RouteRegistry registry, History history, RouteRenderer renderer) {
    Objects.requireNonNull(registry, "RouteRegistry must not be null");
    Objects.requireNonNull(history, "History must not be null");
    Objects.requireNonNull(renderer, "RouteRenderer must not be null");

    this.root = root;
    this.registry = registry;
    this.history = history;
    this.renderer = renderer;
    this.renderer.addObserver(new RouteRendererDispatcher(getEventDispatcher()));
    this.addHistoryStateListener();
  }

  /**
   * Creates a new {@code Router} instance.
   *
   * @param root the base path to use for all routes
   * @param registry the route registry
   * @param history the history object to use for history management
   */
  public Router(String root, RouteRegistry registry, History history) {
    this(root, registry, history, new RouteRenderer(registry));
  }

  /**
   * Creates a new {@code Router} instance.
   *
   * @param root the base path to use for all routes
   * @param registry the route registry
   */
  public Router(String root, RouteRegistry registry) {
    this(root, registry, new BrowserHistory());
  }

  /**
   * Creates a new {@code Router} instance.
   *
   * @param registry the route registry
   * @param history the history object to use for history management
   * @param renderer the route RouteRenderer
   */
  public Router(RouteRegistry registry, History history, RouteRenderer renderer) {
    this(null, registry, history, renderer);
  }

  /**
   * Creates a new {@code Router} instance.
   *
   * @param registry the route registry
   * @param history the history object to use for history management
   */
  public Router(RouteRegistry registry, History history) {
    this(registry, history, new RouteRenderer(registry));
  }

  /**
   * Creates a new {@code Router} instance.
   *
   * @param registry the route registry
   */
  public Router(RouteRegistry registry) {
    this(registry, new BrowserHistory());
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
      Consumer<Optional<? extends Component>> onComplete) {
    Objects.requireNonNull(location, "Location must not be null");

    Location locationRootless = detachRoot(location);
    console().log("Navigating to: " + locationRootless.getFullURI());
    Optional<RoutePattern> routePattern = getRoutePatternForLocation(locationRootless);
    Optional<Class<? extends Component>> componentClass = routePattern.map(RoutePattern::getPattern)
        .map(c -> registry.getComponentByRoute(c).orElse(null));

    if (!routePattern.isPresent()) {
      throw new RouteNotFoundException("Failed to match route for location: " + locationRootless);
    }

    if (!componentClass.isPresent()) {
      throw new RouteNotFoundException(
          "Path matched but no component found for location: " + locationRootless);
    }

    RoutePattern matchedPattern = routePattern.get();
    NavigationContext context =
        new NavigationContext(this, locationRootless, options, matchedPattern, ParametersBag
            .of(matchedPattern.extractParameters(locationRootless.getSegments().getPath())));

    // remove the history state listener to avoid loops
    removeHistoryStateListener();

    renderer.render(componentClass.get(), context, component -> {
      if (component.isPresent()) {
        handleNavigateComplete(component.get(), context);
      }

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
  public void navigate(Location location, Consumer<Optional<? extends Component>> onComplete) {
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
   * @param <T> the type of the component to navigate to
   * @param component the component class to navigate to
   * @param options the navigate options
   * @param routeParameters a map of parameters to be included in the URL
   * @param onComplete the callback to be invoked with the result of the navigation
   */
  public <T extends Component> void navigate(Class<T> component, NavigationOptions options,
      Map<String, String> routeParameters, Consumer<Optional<T>> onComplete) {
    Objects.requireNonNull(component, "Component class must not be null");

    Optional<String> route = registry.getRouteByComponent(component);
    if (!route.isPresent()) {
      throw new RouteNotFoundException(
          "No route found for component: " + component.getSimpleName());
    }

    RoutePattern pattern = getRoutesCache().computeIfAbsent(route.get(), RoutePattern::new);
    String path = pattern.buildUrl(routeParameters != null ? routeParameters : Map.of());
    Location location = new Location(path);

    navigate(location, options, c -> {
      if (onComplete != null && c.isPresent()) {
        onComplete.accept(c.map(component::cast));
      }
    });
  }

  /**
   * Navigates to the location corresponding to the given component with options and parameters.
   *
   * @param <T> the type of the component to navigate to
   * @param component the component class to navigate to
   * @param options the navigate options
   * @param params a map of parameters to be included in the URL
   */
  public <T extends Component> void navigate(Class<T> component, NavigationOptions options,
      Map<String, String> params) {
    navigate(component, options, params, null);
  }

  /**
   * Navigates to the location corresponding to the given component with parameters and a completion
   * callback.
   *
   * @param <T> the type of the component to navigate to
   * @param component the component class to navigate to
   * @param params a map of parameters to be included in the URL
   * @param onComplete the callback to be invoked with the result of the navigation
   */
  public <T extends Component> void navigate(Class<T> component, Map<String, String> params,
      Consumer<Optional<T>> onComplete) {
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
   * @param <T> the type of the component to navigate to
   * @param component the component class to navigate to
   * @param options the navigate options
   * @param onComplete the callback to be invoked with the result of the navigation
   */
  public <T extends Component> void navigate(Class<T> component, NavigationOptions options,
      Consumer<Optional<T>> onComplete) {
    navigate(component, options, null, onComplete);
  }

  /**
   * Navigates to the location corresponding to the given component with options.
   *
   * @param <T> the type of the component to navigate to
   * @param component the component class to navigate to
   * @param options the navigate options
   */
  public <T extends Component> void navigate(Class<T> component, NavigationOptions options) {
    navigate(component, options, null, null);
  }

  /**
   * Navigates to the location corresponding to the given component with a completion callback.
   *
   * @param <T> the type of the component to navigate to
   * @param component the component class to navigate to
   * @param onComplete the callback to be invoked with the result of the navigation
   */
  public <T extends Component> void navigate(Class<T> component, Consumer<Optional<T>> onComplete) {
    navigate(component, new NavigationOptions(), null, onComplete);
  }

  /**
   * Navigates to the location corresponding to the given component.
   *
   * @param <T> the type of the component to navigate to
   * @param component the component class to navigate to
   */
  public <T extends Component> void navigate(Class<T> component) {
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
   * Retrieves the base path.
   *
   * @return the base path
   */
  public String getRoot() {
    return root;
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
    List<RouteEntry> routes = registry.getAvailableRoutes();

    for (RouteEntry route : routes) {
      RoutePattern pattern = getRoutesCache().computeIfAbsent(route.getPath(), RoutePattern::new);
      String currentSegment = location.getSegments().getPath();
      if (pattern.matches(currentSegment)) {
        mp = pattern;
        break;
      }
    }

    return Optional.ofNullable(mp);
  }

  /**
   * Gets the routes cache.
   *
   * @return the routes cache
   */
  protected Map<String, RoutePattern> getRoutesCache() {
    return routesCache;
  }

  /**
   * Adds a history state listener.
   */
  protected void addHistoryStateListener() {
    this.removeHistoryStateListener();
    historyListener =
        history.addHistoryStateChangeListener(e -> e.getLocation().ifPresent(location -> {
          console().log(location.getFullURI());
          navigate(location);
        }));
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
        Location willNavigateToLocation = attachRoot(context.getLocation());
        Location finalWillNavigateToLocation = willNavigateToLocation;

        switch (type) {
          case PUSH:
            history.pushState(state, finalWillNavigateToLocation);
            break;
          case REPLACE:
            history.replaceState(state, finalWillNavigateToLocation);
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

  /**
   * Strips the root from the given location.
   *
   * @param location the location to strip the root from
   * @return the location without the root
   */
  protected Location detachRoot(Location location) {
    String normalizeRoot = normalizePath(getRoot());
    String path = normalizePath(location.getSegments().getPath());

    if (!normalizeRoot.isEmpty() && path.startsWith(normalizeRoot)) {
      path = path.substring(normalizeRoot.length());
      path = normalizePath(path);
    }

    return new Location(new SegmentsBag(path), location.getQueryParameters(),
        location.getFragment());
  }

  /**
   * Adds the root to the given location.
   *
   * @param location the location to add the root to
   * @return the location with the root added
   */
  protected Location attachRoot(Location location) {
    String normalizeRoot = normalizePath(getRoot());
    String path = normalizePath(location.getSegments().getPath());

    if (!normalizeRoot.isEmpty() && !path.startsWith(normalizeRoot)) {
      path = normalizeRoot + "/" + path; // NOSONAR
      path = normalizePath(path);
    }

    // Ensure the path starts with a leading slash
    if (!path.startsWith("/")) {
      path = "/" + path; // NOSONAR
    }

    return new Location(new SegmentsBag(path), location.getQueryParameters(),
        location.getFragment());
  }

  /**
   * Normalizes the given path.
   *
   * @param path the path to normalize
   * @return the normalized path
   */
  protected String normalizePath(String path) {
    if (path == null || path.isEmpty()) {
      return "";
    }

    // Remove leading and trailing slashes and normalize internal double slashes
    return path.replaceAll("/+", "/").replaceAll("^/|/$", ""); // NOSONAR
  }
}