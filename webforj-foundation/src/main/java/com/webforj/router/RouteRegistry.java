package com.webforj.router;

import com.webforj.component.Component;
import com.webforj.component.window.Frame;
import com.webforj.router.annotation.Route;
import com.webforj.router.annotation.RouteAlias;
import io.github.classgraph.ClassGraph;
import io.github.classgraph.ScanResult;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
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
 * @since 24.12
 */
public class RouteRegistry {
  private final Set<RouteEntry> routeConfigs =
      new TreeSet<>(Comparator.comparingInt(RouteEntry::getPriority)
          .thenComparing(RouteEntry::getPath, Comparator.reverseOrder()));

  /**
   * Scans the given base package for classes annotated with {@link Route} and {@link RouteAlias}
   * annotations and builds a new {@link RouteRegistry} instance.
   *
   * @param packages the packages to scan for route-annotated classes
   *
   * @return a new {@link RouteRegistry} instance containing the discovered routes and aliases
   * @throws IllegalStateException if a class annotated with {@link Route} does not extend
   */
  public static RouteRegistry ofPackage(String[] packages) {
    RouteRegistry registry = new RouteRegistry();

    try (ScanResult scanResult =
        new ClassGraph().enableClassInfo().enableAnnotationInfo().acceptPackages(packages).scan()) {

      Set<Class<?>> annotatedClasses = scanResult.getClassesWithAnnotation(Route.class.getName())
          .loadClasses().stream().collect(Collectors.toSet());

      for (Class<?> cls : annotatedClasses) {
        if (Component.class.isAssignableFrom(cls)) {
          registry.register((Class<? extends Component>) cls);
        } else {
          throw new IllegalStateException(
              "Class " + cls.getName() + " does not extend Component but has @Route annotation.");
        }
      }
    }

    return registry;
  }

  /**
   * Registers a route using a {@link RouteEntry}.
   *
   * @param entry the {@link RouteEntry} containing the route details
   * @return the current {@link RouteRegistry} instance
   */
  public RouteRegistry register(RouteEntry entry) {
    routeConfigs.add(entry);
    return this;
  }

  /**
   * Registers a route with the given path and component class.
   *
   * @param route the route path
   * @param component the component that should be rendered when the route is matched
   * @param target the target component of where the component should be rendered
   * @param frameId the frame ID where the component should be rendered
   *
   * @return the current {@link RouteRegistry} instance
   */
  public RouteRegistry register(String route, Class<? extends Component> component,
      Class<? extends Component> target, String frameId) {
    RouteEntry entry = new RouteEntry(route, component, target, frameId, 10);
    return register(entry);
  }

  /**
   * Registers a route with the given path and component class.
   *
   * @param route the route path
   * @param component the component that should be rendered when the route is matched
   * @param target the target component of where the component should be rendered
   *
   * @return the current {@link RouteRegistry} instance
   */
  public RouteRegistry register(String route, Class<? extends Component> component,
      Class<? extends Component> target) {
    return register(route, component, target, null);
  }

  /**
   * Registers a route with the given path and component class.
   *
   * @param route the route path
   * @param component the component that should be rendered when the route is matched
   *
   * @return the current {@link RouteRegistry} instance
   */
  public RouteRegistry register(String route, Class<? extends Component> component) {
    return register(route, component, Frame.class, null);
  }

  /**
   * Registers a route using the annotations present on the component class.
   *
   * @param component the component class to be registered
   * @return the current {@link RouteRegistry} instance
   */
  public RouteRegistry register(Class<? extends Component> component) {
    Route routeAnnotation = component.getAnnotation(Route.class);
    if (routeAnnotation != null) {
      // Process Route annotation
      String routePath = buildFullRoutePath(component);
      RouteEntry entry = new RouteEntry(routePath, component, routeAnnotation.outlet(),
          routeAnnotation.frame(), routeAnnotation.priority());
      register(entry);

      // Process RouteAlias annotations if Route is present
      RouteAlias[] aliases = component.getAnnotationsByType(RouteAlias.class);
      for (RouteAlias alias : aliases) {
        RouteEntry aliasEntry = new RouteEntry(alias.value(), component, routeAnnotation.outlet(),
            routeAnnotation.frame(), alias.priority());

        register(aliasEntry);
      }
    } else {
      throw new IllegalStateException(
          "Class " + component.getName() + " does not have a @Route annotation.");
    }

    return this;
  }

  /**
   * Unregisters the route with the given path.
   *
   * @param route the route path to unregister
   * @return the current {@link RouteRegistry} instance
   */
  public RouteRegistry unregister(String route) {
    routeConfigs.removeIf(entry -> entry.getPath().equals(route));
    return this;
  }

  /**
   * Unregisters the route associated with the given component class.
   *
   * @param componentClass the component class to unregister
   * @return the current {@link RouteRegistry} instance
   */
  public RouteRegistry unregister(Class<? extends Component> componentClass) {
    Optional<String> route = getRouteByComponent(componentClass);
    route.ifPresent(this::unregister);
    return this;
  }

  /**
   * Returns the component class of the passed route.
   *
   * @param path the path of the route
   * @return an Optional containing the component class of the passed route, or an empty Optional if
   *         not found
   */
  public Optional<Class<? extends Component>> getComponentByRoute(String path) {
    return routeConfigs.stream().filter(entry -> entry.getPath().equals(path))
        .<Class<? extends Component>>map(RouteEntry::getComponent).findFirst();
  }

  /**
   * Returns the route path for the given component class.
   *
   * @param component the component class
   * @return an Optional containing the route path for the given component class, or an empty
   *         Optional if not found
   */
  public Optional<String> getRouteByComponent(Class<? extends Component> component) {
    return routeConfigs.stream().filter(entry -> entry.getComponent().equals(component))
        .map(RouteEntry::getPath).map(String::trim).findFirst();
  }

  /**
   * Returns all registered routes.
   *
   * @return a list of all registered routes
   */
  public List<RouteEntry> getAvailableRouteEntires() {
    return routeConfigs.stream().toList();
  }

  /**
   * Returns the outlet class of the component class.
   *
   * @param component the component class
   * @return an Optional containing the outlet class of the component class, or an empty Optional if
   *         not found
   */
  public Optional<Class<? extends Component>> getOutlet(Class<? extends Component> component) {
    return routeConfigs.stream().filter(entry -> entry.getComponent().equals(component))
        .<Class<? extends Component>>map(RouteEntry::getOutlet).filter(Objects::nonNull)
        .findFirst();
  }

  /**
   * Returns the frame ID of the component class.
   *
   * @param component the component class
   * @return an Optional containing the frame ID of the component class, or an empty Optional if not
   *         set
   */
  public Optional<String> getRouteFrameName(Class<? extends Component> component) {
    return routeConfigs.stream().filter(entry -> entry.getComponent().equals(component))
        .map(RouteEntry::getFrameId).filter(Optional::isPresent).map(Optional::get).findFirst();
  }

  /**
   * Returns the root component class of the given component class.
   *
   * @param componentClass the component class
   * @return an Optional containing the root component class of the given component class
   */
  public Optional<RouteRelation<Class<? extends Component>>> getComponentHierarchy(
      Class<? extends Component> componentClass) {
    if (routeConfigs.stream().noneMatch(entry -> entry.getComponent().equals(componentClass))) {
      return Optional.empty();
    }

    // Find the root component for the given component
    LinkedList<Class<? extends Component>> pathComponents = new LinkedList<>();
    populatePathComponents(componentClass, pathComponents);

    if (pathComponents.isEmpty()) {
      return Optional.empty();
    }

    // Build the path tree from root to the given component
    RouteRelation<Class<? extends Component>> rootNode =
        new RouteRelation<>(pathComponents.removeFirst());
    RouteRelation<Class<? extends Component>> currentNode = rootNode;

    for (Class<? extends Component> component : pathComponents) {
      RouteRelation<Class<? extends Component>> childNode = new RouteRelation<>(component);
      currentNode.addChild(childNode);
      currentNode = childNode;
    }

    return Optional.of(rootNode);
  }

  /**
   * Builds the full route path for the given component class, respecting the hierarchy of routes.
   *
   * <p>
   * If the target of a route is another route-annotated class, the path should include the parent
   * route's path as a prefix.
   * </p>
   *
   * @param componentClass the component class for which to build the full route path
   * @return the full route path as a string
   */
  private String buildFullRoutePath(Class<? extends Component> componentClass) {
    Route routeAnnotation = componentClass.getAnnotation(Route.class);

    if (routeAnnotation == null) {
      throw new IllegalArgumentException("Component class must be annotated with @Route.");
    }

    String routePath = routeAnnotation.value();
    Route.Type routeType = routeAnnotation.type();
    boolean isLayout = routeType == Route.Type.LAYOUT;
    boolean isView = routeType == Route.Type.VIEW;
    String className = componentClass.getSimpleName();

    if (routeType == Route.Type.AUTO_DETECT) {
      isLayout = className.endsWith("Layout");
      isView = className.endsWith("View");

      if (!isLayout && !isView) {
        throw new IllegalArgumentException("Cannot AUTO_DETECT route type: "
            + "component class name must end with 'Layout' or 'View'.");
      }
    }

    if (isLayout) {
      String name = Route.AUTO_GENERATED_NAME.equals(routePath)
          ? className.replaceAll("Layout$", "").toLowerCase()
          : routePath;

      if (name.isEmpty()) {
        throw new IllegalArgumentException(
            "Class name 'Layout' is not allowed for generating a route path.");
      }

      if (name.startsWith("@")) {
        return name;
      }
      routePath = "@" + name;
    } else if (isView) {
      String name = Route.AUTO_GENERATED_NAME.equals(routePath)
          ? className.replaceAll("View$", "").toLowerCase()
          : routePath;
      if (routePath.isEmpty()) {
        throw new IllegalArgumentException(
            "Class name 'View' is not allowed for generating a route path.");
      }

      routePath = name;
    }

    Class<? extends Component> outletClass = routeAnnotation.outlet();
    if (outletClass != Frame.class && outletClass != null) {
      Route targetRouteAnnotation = outletClass.getAnnotation(Route.class);
      if (targetRouteAnnotation != null) {
        String targetPath = buildFullRoutePath(outletClass);
        routePath = (targetPath + "/" + routePath).replaceAll("//", "/"); // NOSONAR
      }
    }

    return routePath;
  }

  /**
   * Clears the registry.
   */
  public void clear() {
    routeConfigs.clear();
  }

  private void populatePathComponents(Class<? extends Component> componentClass,
      LinkedList<Class<? extends Component>> pathComponents) {
    Class<? extends Component> currentComponent = componentClass;
    while (currentComponent != null) {
      pathComponents.addFirst(currentComponent);
      Class<? extends Component> targetComponent = getOutlet(currentComponent).orElse(null);
      if (targetComponent == null || targetComponent.equals(currentComponent)) {
        break;
      }
      currentComponent = targetComponent;
    }
  }
}
