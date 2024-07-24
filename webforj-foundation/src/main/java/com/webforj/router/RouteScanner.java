package com.webforj.router;

import com.webforj.component.Component;
import com.webforj.router.annotation.Route;
import io.github.classgraph.ClassGraph;
import io.github.classgraph.ScanResult;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Scans the given base package for classes annotated with {@link Route} and registers them with the
 * given {@link RouteRegistry}.
 */
public final class RouteScanner {

  private RouteScanner() {
    // Prevent instantiation
  }

  /**
   * Scans the given base package for classes annotated with {@link Route} and registers them with
   * the given {@link RouteRegistry}.
   *
   * @param registry the route registry
   * @param basePackage the base package to scan
   */
  public static void scanAndRegisterRoutes(RouteRegistry registry, String basePackage) {
    try (ScanResult scanResult = new ClassGraph().enableClassInfo().enableAnnotationInfo()
        .acceptPackages(basePackage).scan()) {

      Set<Class<?>> annotatedClasses = scanResult.getClassesWithAnnotation(Route.class.getName())
          .loadClasses().stream().collect(Collectors.toSet());

      for (Class<?> cls : annotatedClasses) {
        Route route = cls.getAnnotation(Route.class);
        registry.register(route.value(), (Class<? extends Component>) cls, route.target(),
            route.frame());
      }
    }
  }
}
