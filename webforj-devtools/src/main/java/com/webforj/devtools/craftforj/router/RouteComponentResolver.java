package com.webforj.devtools.craftforj.router;

import com.webforj.component.Component;
import com.webforj.devtools.craftforj.action.CraftforjActionException;
import com.webforj.router.RouteEntry;
import com.webforj.router.RouteRegistry;
import com.webforj.router.Router;

/**
 * Resolves route component classes from the route registry.
 *
 * <p>
 * Client-supplied component type names are honored only when they match a registered route, never
 * through arbitrary class loading.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class RouteComponentResolver {

  private RouteComponentResolver() {}

  /**
   * Resolves the component class registered under the given type name.
   *
   * @param router the router whose registry is searched
   * @param componentType the fully qualified component class name
   * @return the registered route component class
   * @throws CraftforjActionException when the type is not a registered route
   */
  public static Class<? extends Component> resolve(Router router, String componentType) {
    RouteRegistry registry = router.getRegistry();
    if (registry != null) {
      for (RouteEntry entry : registry.getAvailableRouteEntires()) {
        if (entry.getComponent().getName().equals(componentType)) {
          return entry.getComponent();
        }
      }
    }

    throw new CraftforjActionException("Component is not a registered route: " + componentType);
  }
}
