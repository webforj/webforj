package com.webforj.router;

import com.webforj.component.Component;
import com.webforj.router.exception.RouteNotFoundException;

/**
 * Router class responsible for navigating and rendering components based on registered routes.
 */
public class Router {
  private final RouteRegistry registry;
  private final RouteRenderer renderer;

  /**
   * Creates a new {@code Router} instance with the given {@code RouteRegistry}.
   *
   * @param registry the route registry
   */
  public Router(RouteRegistry registry) {
    this.registry = registry;
    this.renderer = new RouteRenderer(registry);
  }

  /**
   * Navigates to the given path.
   *
   * <p>
   * This method navigates to the given path. If the path is registered in the route registry, the
   * component will be rendered. If the path is not registered, a {@code RouteNotFoundException}
   * will be thrown.
   * </p>
   *
   * @param path the path to navigate to
   */
  public void navigate(String path) {
    Class<? extends Component> componentClass = registry.getComponent(path);
    if (componentClass != null) {
      renderer.render(componentClass);
    } else {
      throw new RouteNotFoundException("Route not found: " + path);
    }
  }
}
