package org.dwcj.component;

/**
 * Interface that facilitates implementation of behavior which determines whether or not a control
 * can be disabled after being rendered to the page.
 */
public interface HasEnable {

  /**
   * Returns whether the control is enabled.
   *
   * @return true if the control is enabled, false otherwise
   */
  public Boolean isEnabled();

  /**
   * Set whether the control is to be enabled.
   *
   * @param enabled true if the control is to be enabled, false otherwise
   * @return the control itself
   */
  public HasEnable setEnabled(Boolean enabled);
}
