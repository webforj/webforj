package com.webforj.router;

import com.webforj.component.Component;

/**
 * Represents a route target which is responsible for rendering the given route component.
 *
 * <p>
 * The route target is responsible for rendering the given route component. The target can be a
 * frame or any other component which can render the given component. A component should implement
 * to provide a custom rendering behavior.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public interface RouteTarget {
  /**
   * Renders the given router component.
   *
   * @param component the component to render.
   */
  public void showRouteContent(Component component);

  /**
   * Removes the given router component.
   *
   * @param component the component to remove.
   */
  public void removeRouteContent(Component component);
}
