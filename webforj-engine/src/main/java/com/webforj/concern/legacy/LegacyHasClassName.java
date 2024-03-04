package com.webforj.concern.legacy;


/**
 * Interface facilitating implementation of behavior to add or remove a CSS class to a control.
 *
 * @deprecated Use {@link HasClassName} instead.
 */
@Deprecated(since = "23.05", forRemoval = true)
public interface LegacyHasClassName {

  /**
   * Adds a CSS class to the list of CSS classes for the control.
   *
   * @param className Name of the desired class to be added
   * @return The control itself
   */
  public LegacyHasClassName addClassName(String className);

  /**
   * Removes a CSS class from the list of CSS classes for the control.
   *
   * @param className Name of the desired class to be removed
   * @return The control itself
   */
  public LegacyHasClassName removeClassName(String className);
}
