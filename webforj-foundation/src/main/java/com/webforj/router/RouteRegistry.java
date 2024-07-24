package com.webforj.router;

import com.webforj.component.Component;
import java.util.HashMap;
import java.util.Map;

/**
 * Represents a route registry.
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public class RouteRegistry {
  private Map<String, Class<? extends Component>> routes = new HashMap<>();
  private Map<Class<?>, Class<? extends Component>> targets = new HashMap<>();
  private Map<Class<?>, String> frameIds = new HashMap<>();

  /**
   * Registers a route with the given path and view class.
   *
   * @param path the path of the route
   * @param component the component that should be rendered when the route is matched
   * @param target the target of where the component should be rendered
   * @param frameId the frame ID where the component should be rendered
   */
  public void register(String path, Class<? extends Component> component,
      Class<? extends Component> target, String frameId) {
    routes.put(path, component);

    if (target != null) {
      targets.put(component, target);

      if (frameId != null && !frameId.isEmpty()) {
        frameIds.put(component, frameId);
      }
    }
  }

  /**
   * Registers a route with the given path and view class.
   *
   * @param path the path of the route
   * @param component the component that should be rendered when the route is matched
   * @param target the target of where the component should be rendered
   */
  public void register(String path, Class<? extends Component> component,
      Class<? extends Component> target) {
    register(path, component, target, null);
  }

  /**
   * Registers a route with the given path and view class.
   *
   * @param path the path of the route
   * @param component the component that should be rendered when the route is matched
   */
  public void registerRoute(String path, Class<? extends Component> component) {
    register(path, component, null, null);
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
  public String getFrameId(Class<? extends Component> component) {
    return frameIds.get(component);
  }

  /**
   * Clears the registry.
   */
  public void clear() {
    routes.clear();
    targets.clear();
    frameIds.clear();
  }
}
