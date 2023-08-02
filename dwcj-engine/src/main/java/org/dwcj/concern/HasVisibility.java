package org.dwcj.concern;


/**
 * Interface facilitates implementation of behaviors to modify a controls visibility on a page.
 */
public interface HasVisibility {

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
  public HasVisibility setVisible(Boolean visible);
}
