package com.webforj.router;

import com.webforj.component.Component;

/**
 * Represents a route target which is responsible for rendering the given router component.
 *
 * <p>
 * The route target is responsible for rendering the given router component. The target can be a
 * frame or any other component which can render the given component. A component should implement
 * this interface in order modify the rendering behavior of the router.
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

  /**
   * Removes all the components.
   */
  public void removeAllRouteContent();
}
