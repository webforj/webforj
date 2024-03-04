package com.webforj.concern.legacy;


/**
 * Interface facilitates implementation of behaviors to modify a controls visibility on a page.
 *
 * @deprecated Use {@link HasVisibility} instead.
 */
@Deprecated(since = "23.05", forRemoval = true)
public interface LegacyHasVisibility {

  /**
   * Returns whether the control is visible or invisible.
   *
   * @return if control is visible (=true) or invisible (=false)
   */
  public Boolean isVisible();

  /**
   * Set whether the control is visible or invisible.
   *
   * @param visible if control is visible (=true) or invisible (=false)
   * @return the control itself
   */
  public LegacyHasVisibility setVisible(Boolean visible);
}
