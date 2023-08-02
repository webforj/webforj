package org.dwcj.concern;

/**
 * Interface which facilitates implementation of behavior that allows for the destruction of a
 * control.
 */
public interface HasDestroy {

  /**
   * Destroy the control and remove it from the panel.
   */
  public void destroy();

  /**
   * Returns whether or not the control is destroyed.
   *
   * @return True if the control will is destroyed, false otherwise
   */
  public Boolean isDestroyed();
}
