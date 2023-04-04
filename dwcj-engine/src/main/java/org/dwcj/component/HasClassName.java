package org.dwcj.component;


/**
 * Interface facilitating implementation of behavior to add or remove a CSS class to a control
 */
public interface HasClassName {

  /**
   * Adds a CSS class to the list of CSS classes for the control.
   *
   * @param String name of the desired class to be added
   * @return The control itself
   */
  public HasClassName addClassName(String className);

  /**
   * Removes a CSS class from the list of CSS classes for the control.
   *
   * @param String name of the desired class to be removed
   * @return The control itself
   */
  public HasClassName removeClassName(String className);
}
