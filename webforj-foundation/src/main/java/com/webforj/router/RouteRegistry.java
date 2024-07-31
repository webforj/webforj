package com.webforj.router;

import com.webforj.component.Component;
import com.webforj.component.window.Frame;
import com.webforj.data.tree.Vnode;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * Represents a route registry.
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public class RouteRegistry {
  private Map<String, Class<? extends Component>> routes = new ConcurrentHashMap<>();
  private Map<Class<?>, Class<? extends Component>> targets = new ConcurrentHashMap<>();
  private Map<Class<?>, String> frameIds = new ConcurrentHashMap<>();
  private Map<Class<? extends Component>, Vnode<Class<? extends Component>>> componentsTree =
      new ConcurrentHashMap<>();

  /**
   * Registers a route with the given path and component class.
   *
   * @param route the route path
   * @param component the component that should be rendered when the route is matched
   * @param target the target component of where the component should be rendered
   * @param frameId the frame ID where the component should be rendered
   */
  public void register(String route, Class<? extends Component> component,
      Class<? extends Component> target, String frameId) {
    routes.put(route, component);
    targets.put(component, target);

    if (frameId != null && !frameId.isEmpty() && Frame.class.isAssignableFrom(target)) {
      frameIds.put(component, frameId);
    }

    // Build the component relationship tree
    Vnode<Class<? extends Component>> componentTree =
        componentsTree.computeIfAbsent(component, Vnode::new);
    if (target != null) {
      Vnode<Class<? extends Component>> targetTree =
          componentsTree.computeIfAbsent(target, Vnode::new);
      targetTree.addChild(componentTree);
    }
  }

  /**
   * Registers a route with the given path and component class.
   *
   * @param route the route path
   * @param component the component that should be rendered when the route is matched
   * @param target the target component of where the component should be rendered
   */
  public void register(String route, Class<? extends Component> component,
      Class<? extends Component> target) {
    register(route, component, target, null);
  }

  /**
   * Registers a route with the given path and component class.
   *
   * @param route the route path
   * @param component the component that should be rendered when the route is matched
   */
  public void register(String route, Class<? extends Component> component) {
    register(route, component, Frame.class, null);
  }

  /**
   * Returns the component class of the passed route as registered in the registry.
   *
   * <p>
   * Passing the full path of the route will return not return the component class of the route. The
   * route should be the path as registered in the registry. for example, if the route is registered
   * as "/users/:id", the path should be "/users/:id"
   * </p>
   *
   * @param path the path of the route
   * @return the component class of the passed route
   */
  public Class<? extends Component> getComponentByRoute(String path) {
    return routes.get(path);
  }

  /**
   * Returns the route path for the given component class as registered in the registry.
   *
   * @param component the component class
   * @return the route path for the given component class
   */
  public String getRouteByComponent(Class<? extends Component> component) {
    return routes.entrySet().stream().filter(entry -> entry.getValue().equals(component))
        .map(Map.Entry::getKey).findFirst().orElse(null);
  }

  /**
   * Returns the full resolved path of the route for the given component class.
   *
   * <p>
   * The resolved path is the full path of the route including the parent components. for example,
   * if the route is registered as "/users/edit/:id" and the parent component is "/users", the
   * resolved path will be "/users/edit/:id".
   * </p>
   *
   * @param component the component class
   * @return the full path of the route with the given component class
   */
  public String getResolvedRouteByComponent(Class<? extends Component> component) {
    LinkedList<String> pathComponents = new LinkedList<>();
    Class<? extends Component> currentComponent = component;

    while (currentComponent != null) {
      String path = getRouteByComponent(currentComponent);
      if (path != null) {
        pathComponents.addFirst(path);
      }
      currentComponent = targets.get(currentComponent);
    }

    return String.join("/", pathComponents);
  }

  /**
   * Returns the component class for the given full path by resolving the full path dynamically.
   *
   * @param fullPath the full path of the route
   * @return the component class associated with the full path, or null if not found
   */
  public Class<? extends Component> getResolvedComponentByRoute(String fullPath) {
    for (Map.Entry<String, Class<? extends Component>> entry : routes.entrySet()) {
      if (getResolvedRouteByComponent(entry.getValue()).equals(fullPath)) {
        return entry.getValue();
      }
    }
    return null;
  }

  /**
   * Returns the full paths of all registered routes.
   *
   * @return a list of full paths of all registered routes
   */
  public List<String> getResolvedRoutes() {
    return routes.keySet().stream().map(route -> getResolvedRouteByComponent(routes.get(route)))
        .distinct().collect(Collectors.toList());
  }

  /**
   * Returns the parent class of the component class.
   *
   * @param component the component class
   * @return the parent class of the component class
   */
  public Class<? extends Component> getTarget(Class<? extends Component> component) {
    return targets.get(component);
  }

  /**
   * Returns the frame ID of the view class.
   *
   * @param component the component class
   * @return the frame ID of the view class
   */
  public String getFrameRouteId(Class<? extends Component> component) {
    return frameIds.get(component);
  }

  /**
   * Returns the root component class of the given component class.
   *
   * @param componentClass the component class
   * @return the root component class of the given component class
   */
  public Optional<Vnode<Class<? extends Component>>> getComponentsTree(
      Class<? extends Component> componentClass) {
    // Find the root component for the given component
    Class<? extends Component> currentComponent = componentClass;
    LinkedList<Class<? extends Component>> pathComponents = new LinkedList<>();

    while (targets.containsKey(currentComponent)) {
      pathComponents.addFirst(currentComponent);
      currentComponent = targets.get(currentComponent);
    }
    pathComponents.addFirst(currentComponent); // add the root component

    // Build the path tree from root to the given component
    Vnode<Class<? extends Component>> rootNode = new Vnode<>(pathComponents.removeFirst());
    Vnode<Class<? extends Component>> currentNode = rootNode;

    for (Class<? extends Component> component : pathComponents) {
      Vnode<Class<? extends Component>> childNode = new Vnode<>(component);
      currentNode.addChild(childNode);
      currentNode = childNode;
    }

    return Optional.of(rootNode);
  }

  /**
   * Clears the registry.
   */
  public void clear() {
    routes.clear();
    targets.clear();
    frameIds.clear();
    componentsTree.clear();
  }
}
