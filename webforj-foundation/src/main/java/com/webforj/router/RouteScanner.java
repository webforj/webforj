package com.webforj.router;

import com.webforj.component.Component;
import com.webforj.component.window.Frame;
import com.webforj.router.annotation.Route;
import com.webforj.router.annotation.RouteAlias;
import io.github.classgraph.ClassGraph;
import io.github.classgraph.ScanResult;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * The {@code RouteScanner} class is responsible for scanning a specified base package for classes
 * annotated with {@link Route} and {@link RouteAlias}, and providing utilities to register them
 * with a {@link RouteRegistry}.
 *
 * <p>
 * This class facilitates the automatic discovery of route components in the application by
 * identifying classes annotated with {@link Route} and {@link RouteAlias}. It processes these
 * classes by extracting the route paths, determining view names if specified, and prioritizing the
 * routes based on their defined priority. The discovered routes can then be registered in the
 * provided {@link RouteRegistry}.
 * </p>
 *
 * @since 24.11
 */
public final class RouteScanner {

  private RouteScanner() {
    // Prevent instantiation
  }

  /**
   * Scans the given base package for classes annotated with {@link Route} and {@link RouteAlias}.
   *
   * @param basePackage the base package to scan for route-annotated classes
   * @return a list of {@link RouteEntry} representing the discovered routes and aliases
   */
  public static List<RouteEntry> scanForRoutes(String basePackage) {
    List<RouteEntry> routes = new ArrayList<>();

    try (ScanResult scanResult = new ClassGraph().enableClassInfo().enableAnnotationInfo()
        .acceptPackages(basePackage).scan()) {

      Set<Class<?>> annotatedClasses = scanResult.getClassesWithAnnotation(Route.class.getName())
          .loadClasses().stream().collect(Collectors.toSet());

      for (Class<?> cls : annotatedClasses) {
        Route routeAnnotation = cls.getAnnotation(Route.class);
        RouteAlias[] aliases = cls.getAnnotationsByType(RouteAlias.class);

        // Check if the class extends Component when Route is present
        if (routeAnnotation != null) {
          if (!Component.class.isAssignableFrom(cls)) {
            throw new IllegalStateException(
                "Class " + cls.getName() + " does not extend Component but has @Route annotation.");
          }

          // Process Route annotation
          String routePath = buildFullRoutePath((Class<? extends Component>) cls);
          routes.add(new RouteEntry(routePath, (Class<? extends Component>) cls,
              routeAnnotation.target(), routeAnnotation.frame(), routeAnnotation.priority()));

          // Process RouteAlias annotations if Route is present
          for (RouteAlias alias : aliases) {
            routes.add(new RouteEntry(alias.value(), (Class<? extends Component>) cls,
                routeAnnotation.target(), routeAnnotation.frame(), alias.priority()));
          }
        }
      }
    }

    return routes;
  }

  /**
   * Registers the provided list of {@link RouteEntry} objects with the specified
   * {@link RouteRegistry}.
   *
   * @param registry the route registry where discovered routes will be registered
   * @param routes the list of routes to register
   */
  public static void registerRoutes(RouteRegistry registry, List<RouteEntry> routes) {
    for (RouteEntry entry : routes) {
      registry.register(entry);
    }
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
  private static String buildFullRoutePath(Class<? extends Component> componentClass) {
    Route routeAnnotation = componentClass.getAnnotation(Route.class);
    String routePath = routeAnnotation.value();

    // Generate a view name if the route path is auto-generated
    if (Route.AUTO_GENERATED_VIEW_NAME.equals(routePath)) {
      routePath = ViewNameGenerator.generate(componentClass);
    }

    Class<? extends Component> targetClass = routeAnnotation.target();
    if (targetClass != Frame.class && targetClass != null) {
      Route targetRouteAnnotation = targetClass.getAnnotation(Route.class);
      if (targetRouteAnnotation != null) {
        String targetPath = buildFullRoutePath(targetClass);
        routePath = (targetPath + "/" + routePath).replaceAll("//", "/"); // NOSONAR
      }
    }

    return routePath;
  }
}
