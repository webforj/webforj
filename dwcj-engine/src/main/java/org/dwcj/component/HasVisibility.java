package org.dwcj.component;


/**
 * Interface facilitates implementation of behaviors to modify a controls visibility on a page
 */
public interface HasVisibility {

  /**
   *
   * @return if control is visible (=true) or invisible (=false)
   */
  public Boolean isVisible();

  /**
   * Set whether the control is visible or invisible
   *
   * @param visible
   * @return the control itself
   */
  public HasVisibility setVisible(Boolean visible);
}
