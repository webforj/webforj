package com.webforj.router;

import com.webforj.component.Component;
import com.webforj.component.window.Frame;
import java.util.Objects;
import java.util.Optional;

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
  private final String path;
  private final Class<? extends Component> component;
  private final Class<? extends Component> target;
  private final int priority;
  private String frameId = null;

  /**
   * Constructs a new {@code RouteEntry} instance.
   *
   * @param path the path for the route
   * @param component the component class associated with the route
   * @param target the target class for the route, if any
   * @param frameId the frame ID associated with the route, if any
   * @param priority the priority of the route
   */
  public RouteEntry(String path, Class<? extends Component> component,
      Class<? extends Component> target, String frameId, int priority) {
    Objects.requireNonNull(path, "Route path cannot be null");
    Objects.requireNonNull(component, "Route component class cannot be null");
    Objects.requireNonNull(target, "Route target class cannot be null");

    this.path = path;
    this.component = component;
    this.target = target;
    this.priority = priority;

    Optional.ofNullable(frameId).ifPresent(id -> {
      if (!id.isEmpty() && Frame.class.isAssignableFrom(target)) {
        this.frameId = id;
      }
    });
  }

  /**
   * Constructs a new {@code RouteEntry} instance.
   *
   * @param path the path for the route
   * @param component the component class associated with the route
   * @param target the target class for the route, if any
   * @param priority the priority of the route
   */
  public RouteEntry(String path, Class<? extends Component> component,
      Class<? extends Component> target, int priority) {
    this(path, component, target, null, priority);
  }

  /**
   * Constructs a new {@code RouteEntry} instance with the frame class as the target.
   *
   * @param path the path for the route
   * @param component the component class associated with the route
   * @param priority the priority of the route
   */
  public RouteEntry(String path, Class<? extends Component> component, int priority) {
    this(path, component, Frame.class, null, priority);
  }

  /**
   * Constructs a new {@code RouteEntry} instance with the frame class as the target.
   *
   * @param path the path for the route
   * @param component the component class associated with the route
   * @param priority the priority of the route
   * @param frameId the frame ID associated with the route, if any
   */
  public RouteEntry(String path, Class<? extends Component> component, int priority,
      String frameId) {
    this(path, component, priority);
  }

  /**
   * Constructs a new {@code RouteEntry} instance with the frame class as the target and a default
   * priority of 10.
   *
   * @param path the path for the route
   * @param component the component class associated with the route
   */
  public RouteEntry(String path, Class<? extends Component> component) {
    this(path, component, 10);
  }

  /**
   * Returns the path for the route.
   *
   * @return the route path
   */
  public String getPath() {
    return path;
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
  public Optional<String> getFrameId() {
    return Optional.ofNullable(frameId);
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
