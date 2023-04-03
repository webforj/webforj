package org.dwcj.component;

/**
 * Interface facilitating implementation of behavior to modify a specific
 * CSS property to a provided value.
 */
public interface HasStyle {

  /**
   * Get the value of a CSS property.
   * 
   * This method is intended to be used to retrieve the value of a CSS property
   * of a control.
   * 
   * @param property The CSS property to be retrieved
   * @return String containing the value of the CSS property
   */
  public String getStyle(String property);

  /**
   * Get the computed value of a CSS property.
   * 
   * This method is intended to be used to retrieve the computed value of a
   * CSS property of a control.
   * 
   * @param property The CSS property to be retrieved
   * @return String containing all computed styles
   */
  public String getComputedStyle(String property);

  /**
   * Set a CSS property to a specific value.
   * 
   * This method is intended to be used to modify a single CSS property of a
   * control.
   * 
   * @param property The CSS property to be changed
   * @param value    The value to be assigned to the CSS property
   * 
   * @return The control itself
   */
  public HasStyle setStyle(String property, String value);

  /**
   * Removes a CSS property to a specific value.
   * 
   * @param property The CSS property to be changed
   * @param value    The value to be assigned to the CSS property
   * 
   * @return The control itself
   */
  public HasStyle removeStyle(String property);
}
