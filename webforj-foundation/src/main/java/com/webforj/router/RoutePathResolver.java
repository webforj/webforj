package com.webforj.router;

import com.webforj.component.Component;
import com.webforj.component.window.Frame;
import com.webforj.router.annotation.Route;

/**
 * Utility class to resolve route paths from component classes.
 *
 * <p>
 * This utility extracts and builds complete route paths from components annotated with
 * {@link Route}, respecting the hierarchy of routes and their outlets.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.10
 */
public final class RoutePathResolver {

  private RoutePathResolver() {
    // Utility class
  }

  /**
   * Resolves the complete route path for a component class by traversing its route hierarchy.
   *
   * <p>
   * If the target of a route is another route-annotated class, the path includes the parent route's
   * path as a prefix.
   * </p>
   *
   * @param componentClass the component class for which to build the full route path
   * @return the full route path as a string, or null if the component doesn't have a @Route
   *         annotation
   * @throws IllegalArgumentException if route type cannot be auto-detected or class name is invalid
   */
  public static String resolvePath(Class<? extends Component> componentClass) {
    if (componentClass == null) {
      return null;
    }

    Route routeAnnotation = componentClass.getAnnotation(Route.class);
    if (routeAnnotation == null) {
      return null;
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
        throw new IllegalArgumentException("Cannot AUTO_DETECT route type for component class '"
            + className + "': " + "component class name must end with 'Layout' or 'View'.");
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
        String targetPath = resolvePath(outletClass);
        routePath = (targetPath + "/" + routePath).replaceAll("//", "/"); // NOSONAR
      }
    }

    return routePath;
  }
}
