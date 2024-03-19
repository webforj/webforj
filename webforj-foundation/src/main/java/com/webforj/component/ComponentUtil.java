package com.webforj.component;

/**
 * Utility methods for {@link Component}.
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public class ComponentUtil {

  private ComponentUtil() {}

  /**
   * Retrieves the bound component of the given component.
   *
   * @param component the component to retrieve its bound component
   * @return the bound component
   */
  public static Component getBoundComponent(Object component) {
    if (!(component instanceof Composite)) {
      throw new IllegalArgumentException("The component must be an instance of Composite");
    }

    return ((Composite<?>) component).getBoundComponent();
  }
}
