package org.dwcj.concern;

import org.dwcj.component.Component;

/**
 * An interface for handling CSS styles on a component.
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public interface HasStyle<T extends Component> {

  /**
   * Gets the value of a CSS property.
   *
   * <p>
   * This method is intended to be used to retrieve the value of a CSS property of a component.
   * </p>
   *
   * @see #getComputedStyle(java.lang.String)
   *
   * @param property The CSS property to be retrieved
   * @return String containing the value of the CSS property
   */
  public String getStyle(String property);

  /**
   * Gets the computed value of a CSS property.
   *
   * <p>
   * This method is used to obtain the computed value of a CSS property for the component. The
   * computed value represents the final value that is applied to the element after considering all
   * styles applied to it, including styles inherited from parent elements and user-agent defaults.
   * </p>
   *
   * @see #getStyle(java.lang.String)
   *
   * @param property The CSS property to be retrieved
   * @return String containing all computed styles
   */
  public String getComputedStyle(String property);

  /**
   * Sets a CSS property to a specific value.
   *
   * <p>
   * This method is intended to be used to modify a single CSS property of a component.
   * </p>
   *
   * @param property The CSS property to be changed
   * @param value The value to be assigned to the CSS property
   *
   * @return @return The component itself.
   */
  public T setStyle(String property, String value);

  /**
   * Removes a CSS property to a specific value.
   *
   * @param property The CSS property to be changed
   *
   * @return @return The component itself.
   */
  public T removeStyle(String property);
}
