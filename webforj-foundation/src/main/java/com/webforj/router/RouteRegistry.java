package com.webforj.router;

import com.webforj.component.Component;
import com.webforj.component.window.Frame;
import com.webforj.data.tree.Vnode;
import java.util.LinkedList;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Represents a route registry.
 */
public class RouteRegistry {
  private Map<String, Class<? extends Component>> routes = new ConcurrentHashMap<>();
  private Map<Class<?>, Class<? extends Component>> targets = new ConcurrentHashMap<>();
  private Map<Class<?>, String> frameIds = new ConcurrentHashMap<>();
  private Map<Class<? extends Component>, Vnode<Class<? extends Component>>> tree =
      new ConcurrentHashMap<>();

  /**
   * Registers a route with the given path and component class.
   *
   * @param path the path of the route
   * @param component the component that should be rendered when the route is matched
   * @param target the target component of where the component should be rendered
   * @param frameId the frame ID where the component should be rendered
   */
  public void register(String path, Class<? extends Component> component,
      Class<? extends Component> target, String frameId) {
    routes.put(path, component);
    targets.put(component, target);

    if (frameId != null && !frameId.isEmpty() && Frame.class.isAssignableFrom(target)) {
      frameIds.put(component, frameId);
    }

    // Build the component relationship tree
    Vnode<Class<? extends Component>> componentTree = tree.computeIfAbsent(component, Vnode::new);
    if (target != null) {
      Vnode<Class<? extends Component>> targetTree = tree.computeIfAbsent(target, Vnode::new);
      targetTree.addChild(componentTree);
    }
  }

  /**
   * Registers a route with the given path and component class.
   *
   * @param path the path of the route
   * @param component the component that should be rendered when the route is matched
   * @param target the target component of where the component should be rendered
   */
  public void register(String path, Class<? extends Component> component,
      Class<? extends Component> target) {
    register(path, component, target, null);
  }

  /**
   * Registers a route with the given path and component class.
   *
   * @param path the path of the route
   * @param component the component that should be rendered when the route is matched
   */
  public void register(String path, Class<? extends Component> component) {
    register(path, component, Frame.class, null);
  }

  /**
   * Returns the component class of the route with the given path.
   *
   * @param path the path of the route
   * @return the component class of the route with the given path
   */
  public Class<? extends Component> getComponent(String path) {
    return routes.get(path);
  }

  /**
   * Returns the path of the route with the given component class.
   *
   * @param component the component class
   * @return the path of the route with the given component class
   */
  public String getRoute(Class<? extends Component> component) {
    return routes.entrySet().stream().filter(entry -> entry.getValue().equals(component))
        .map(Map.Entry::getKey).findFirst().orElse(null);
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
   * Returns the tree path from the root to the given component class.
   *
   * @param componentClass the component class
   * @return the tree path from the root to the given component class
   */
  public Optional<Vnode<Class<? extends Component>>> getPathTree(
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
    tree.clear();
  }
}
