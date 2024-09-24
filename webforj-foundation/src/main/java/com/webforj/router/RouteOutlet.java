package com.webforj.router;

import com.webforj.component.Component;

/**
 * Represents a route target which is responsible for rendering the given route component.
 *
 * <p>
 * The route outlet is responsible for rendering the given route component. The outlet can be a
 * frame or any other component which can render the given component. A component should implement
 * this interface to provide a custom rendering behavior.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.12
 */
public interface RouteOutlet {
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
