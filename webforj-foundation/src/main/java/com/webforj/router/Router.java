package com.webforj.router;

import java.util.List;
import java.util.Objects;
import com.webforj.component.Component;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.router.exception.RouteNotFoundException;
import com.webforj.router.history.History;
import com.webforj.router.history.MemoryHistory;
import com.webforj.router.history.event.HistoryStateChangeEvent;

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
    this.addHistoryStateListener();
  }

  /**
   * Creates a new {@code Router} instance with the given {@code RouteRegistry} and {@code History}
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
   * Navigates to the given path.
   *
   * <p>
   * This method navigates to the given path. If the path can not be found, a
   * {@code RouteNotFoundException} will be thrown.
   * </p>
   *
   * @param path the path to navigate to
   * @return the router instance
   */
  public Router navigate(String path) {
    Class<? extends Component> matchedComponent = null;
    String matchedRoute = null;

    List<String> routes = registry.getResolvedRoutes();
    // RouteCompiler compiler = new RouteCompiler();
    // RouteMatcher matcher = new RouteMatcher();

    // for (String route : routes) {
    // if (matcher.matches(path, compiler.compile(route))) {
    // matchedRoute = route;
    // matchedComponent = registry.getComponentByRoute(route);
    // break;
    // }
    // }

    if (matchedComponent != null) {
      this.removeHistoryStateListener();
      renderer.navigate(matchedComponent);
      history.pushState(new Location(path));
      this.addHistoryStateListener();
    } else {
      throw new RouteNotFoundException("Route not found: " + path);
    }

    return this;
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

  private void addHistoryStateListener() {
    this.removeHistoryStateListener();
    historyListener = history.addHistoryStateChangeListener(
        e -> e.getLocation().ifPresent(l -> navigate(l.getFullURI())));
  }

  private void removeHistoryStateListener() {
    if (historyListener != null) {
      historyListener.remove();
    }
  }
}
