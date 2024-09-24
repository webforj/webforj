package com.webforj.router;

import com.webforj.component.Component;
import com.webforj.component.window.Frame;
import com.webforj.router.history.Location;
import com.webforj.router.history.ParametersBag;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Optional;
import java.util.Set;

/**
 * The {@code NavigationContext} class encapsulates the context information for a navigation event
 * within the router. It contains essential details such as the router instance, location,
 * navigation options, route pattern, and route parameters.
 *
 * <p>
 * This context is used throughout the routing process to provide a consistent set of data that
 * defines the navigation event. It is particularly useful for managing the state and options
 * related to a specific navigation action, enabling various components and observers to access this
 * information as needed.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.12
 */
public class NavigationContext {
  private final Router router;
  private Component component;
  private final Location location;
  private final NavigationOptions options;
  private final RoutePattern routePattern;
  private final ParametersBag routeParameters;
  private final LinkedHashSet<Component> components = new LinkedHashSet<>();
  private Frame activeFrame;

  /**
   * Constructs a new {@code NavigationContext} with the specified parameters.
   *
   * @param router the router instance handling the navigation
   * @param component the target component for the navigation
   * @param location the target location for the navigation
   * @param options the navigation options, which may define additional behaviors like history
   *        update and event firing
   * @param routePattern the pattern that matches the route being navigated to
   * @param routeParameters the parameters extracted from the route, often used for dynamic routing
   */
  public NavigationContext(Router router, Component component, Location location,
      NavigationOptions options, RoutePattern routePattern, ParametersBag routeParameters) {
    this.router = router;
    this.component = component;
    this.location = location;
    this.options = options;
    this.routePattern = routePattern;
    this.routeParameters = routeParameters;
  }

  /**
   * Constructs a new {@code NavigationContext} with the specified parameters.
   */
  NavigationContext() {
    this(null, null, null, null, null, null);
  }

  /**
   * Returns the router associated with this navigation context.
   *
   * @return the {@link Router} instance managing the navigation
   */
  public Router getRouter() {
    return router;
  }

  /**
   * Returns the location associated with this navigation context.
   *
   * @return the {@link Location} object representing the target of the navigation
   */
  public Location getLocation() {
    return location;
  }

  /**
   * Returns the navigation options for this context, if present.
   *
   * <p>
   * These options may include configurations such as whether to update the history, fire events, or
   * invoke observers during the navigation process.
   * </p>
   *
   * @return an {@link Optional} containing the {@link NavigationOptions} if they are specified,
   *         otherwise an empty {@code Optional}
   */
  public Optional<NavigationOptions> getOptions() {
    return Optional.ofNullable(options);
  }

  /**
   * Returns the route pattern that matches the current route.
   *
   * <p>
   * The route pattern is used to determine which route definition applies to the current
   * navigation, helping to identify the target component and associated behaviors.
   * </p>
   *
   * @return the {@link RoutePattern} object for the matched route
   */
  public RoutePattern getRoutePattern() {
    return routePattern;
  }

  /**
   * Returns the parameters extracted from the route.
   *
   * <p>
   * These parameters are often used for dynamic routes where parts of the URL may vary. For
   * example, a route defined as "/user/:id" will extract the "id" parameter from the URL and make
   * it available through this method.
   * </p>
   *
   * @return the {@link ParametersBag} containing the route parameters
   */
  public ParametersBag getRouteParameters() {
    return routeParameters;
  }

  /**
   * Returns the target component for this navigation context.
   *
   * @return the {@link Component} class representing the target of the navigation
   */
  public Component getComponent() {
    return component;
  }

  /**
   * Returns the list of components that has been created or reused during the navigation process.
   *
   * @return the list of {@link Component} classes that have been navigated to
   */
  public Set<Component> getAllComponents() {
    return Collections.unmodifiableSet(components);
  }

  /**
   * Adds a component to the list of navigated components.
   *
   * @param component the {@link Component} class to add to the list
   */
  void addComponent(Component component) {
    components.add(component);
  }

  /**
   * Returns the active frame associated with this navigation context.
   *
   * <p>
   * The active frame is the frame that is currently the container for the navigated components.
   * </p>
   *
   * @return the {@link Frame} object representing the active frame
   */
  public Optional<Frame> getActiveFrame() {
    return Optional.ofNullable(activeFrame);
  }

  /**
   * Sets the active frame for this navigation context.
   *
   * @param activeFrame the {@link Frame} object to set as the active frame
   */
  void setActiveFrame(Frame activeFrame) {
    this.activeFrame = activeFrame;
  }

  /**
   * Sets the target component for this navigation context.
   *
   * @param component the {@link Component} class representing the target of the navigation
   */
  void setComponent(Component component) {
    this.component = component;
  }
}
