package com.webforj.router;

import com.webforj.component.Component;

/**
 * A class to hold route information.
 *
 * <p>
 * The {@code RouteEntry} class encapsulates the details of a route, including the route path,
 * component class, target class, frame ID, and priority. It is used to manage and sort routes
 * before registration.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public class RouteEntry {
  private final String routePath;
  private final Class<? extends Component> component;
  private final Class<? extends Component> target;
  private final String frameId;
  private final int priority;

  /**
   * Constructs a new {@code RouteEntry} instance with the specified parameters.
   *
   * @param routePath the path for the route
   * @param componentClass the component class associated with the route
   * @param targetClass the target class for the route, if any
   * @param frameId the frame ID associated with the route, if any
   * @param priority the priority of the route
   */
  public RouteEntry(String routePath, Class<? extends Component> componentClass,
      Class<? extends Component> targetClass, String frameId, int priority) {
    this.routePath = routePath;
    this.component = componentClass;
    this.target = targetClass;
    this.frameId = frameId;
    this.priority = priority;
  }

  /**
   * Returns the path for the route.
   *
   * @return the route path
   */
  public String getValue() {
    return routePath;
  }

  /**
   * Returns the component class associated with the route.
   *
   * @return the component class
   */
  public Class<? extends Component> getComponent() {
    return component;
  }

  /**
   * Returns the target class for the route, if any.
   *
   * @return the target class, or {@code null} if none
   */
  public Class<? extends Component> getTarget() {
    return target;
  }

  /**
   * Returns the frame ID associated with the route, if any.
   *
   * @return the frame ID, or {@code null} if none
   */
  public String getFrameId() {
    return frameId;
  }

  /**
   * Returns the priority of the route.
   *
   * @return the route priority
   */
  public int getPriority() {
    return priority;
  }
}
