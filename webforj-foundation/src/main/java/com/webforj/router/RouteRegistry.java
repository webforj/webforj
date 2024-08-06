package com.webforj.router;

import com.webforj.component.Component;
import com.webforj.component.window.Frame;
import com.webforj.data.tree.Vnode;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * Represents a route registry.
 *
 * <p>
 * This class manages the registration and retrieval of routes, including their associated
 * components, target classes, and frame IDs. It also maintains the hierarchy of components and
 * supports various methods for querying the registered routes.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public class RouteRegistry {
  private final Map<String, RouteEntry> routeEntries = new ConcurrentHashMap<>();
  private final Map<Class<? extends Component>, Vnode<Class<? extends Component>>> componentsTree =
      new ConcurrentHashMap<>();

  /**
   * Registers a route using a {@link RouteEntry}.
   *
   * @param entry the {@link RouteEntry} containing the route details
   */
  public void register(RouteEntry entry) {
    String route = entry.getPath();
    routeEntries.put(route, entry);

    Class<? extends Component> component = entry.getComponent();
    Class<? extends Component> target = entry.getTarget();

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
   * @param frameId the frame ID where the component should be rendered
   */
  public void register(String route, Class<? extends Component> component,
      Class<? extends Component> target, String frameId) {
    RouteEntry entry = new RouteEntry(route, component, target, frameId, 10);
    register(entry);
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
   * Returns the component class of the passed route.
   *
   * @param path the path of the route
   * @return an Optional containing the component class of the passed route, or an empty Optional if
   *         not found
   */
  public Optional<Class<? extends Component>> getComponentByRoute(String path) {
    return Optional.ofNullable(routeEntries.get(path)).map(RouteEntry::getComponent);
  }

  /**
   * Returns the route path for the given component class.
   *
   * @param component the component class
   * @return an Optional containing the route path for the given component class, or an empty
   *         Optional if not found
   */
  public Optional<String> getRouteByComponent(Class<? extends Component> component) {
    return routeEntries.values().stream().filter(entry -> entry.getComponent().equals(component))
        .map(RouteEntry::getPath).findFirst();
  }

  /**
   * Returns the full paths of all registered routes, sorted by priority.
   *
   * @return a list of full paths of all registered routes
   */
  public List<String> getAvailableRoutes() {
    return routeEntries.values().stream().sorted(Comparator.comparingInt(RouteEntry::getPriority))
        .map(RouteEntry::getPath).collect(Collectors.toList());
  }

  /**
   * Returns the target class of the component class.
   *
   * @param component the component class
   * @return an Optional containing the target class of the component class, or an empty Optional if
   *         not found
   */
  public Optional<Class<? extends Component>> getTarget(Class<? extends Component> component) {
    Optional<?> found =
        routeEntries.values().stream().filter(entry -> entry.getComponent().equals(component))
            .map(RouteEntry::getTarget).filter(Objects::nonNull).findFirst();

    return found.map(target -> (Class<? extends Component>) target);
  }

  /**
   * Returns the frame ID of the component class.
   *
   * @param component the component class
   * @return an Optional containing the frame ID of the component class, or an empty Optional if not
   *         set
   */
  public Optional<String> getFrameRouteId(Class<? extends Component> component) {
    return routeEntries.values().stream().filter(entry -> entry.getComponent().equals(component))
        .map(RouteEntry::getFrameId).filter(Optional::isPresent).map(Optional::get).findFirst();
  }

  /**
   * Returns the root component class of the given component class.
   *
   * @param componentClass the component class
   * @return an Optional containing the root component class of the given component class
   */
  public Optional<Vnode<Class<? extends Component>>> getComponentsTree(
      Class<? extends Component> componentClass) {
    // Find the root component for the given component
    LinkedList<Class<? extends Component>> pathComponents = new LinkedList<>();
    populatePathComponents(componentClass, pathComponents);

    if (pathComponents.isEmpty()) {
      return Optional.empty();
    }

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

  private void populatePathComponents(Class<? extends Component> componentClass,
      LinkedList<Class<? extends Component>> pathComponents) {
    Class<? extends Component> currentComponent = componentClass;
    while (currentComponent != null) {
      pathComponents.addFirst(currentComponent);
      Class<? extends Component> targetComponent = getTarget(currentComponent).orElse(null);
      if (targetComponent == null || targetComponent.equals(currentComponent)) {
        break;
      }
      currentComponent = targetComponent;
    }
  }

  /**
   * Clears the registry.
   */
  public void clear() {
    routeEntries.clear();
    componentsTree.clear();
  }
}
