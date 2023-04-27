package org.dwcj.component;

/**
 * Interface that facilitates implementation of behavior which lets a component be focused.
 */
public interface HasFocus {

  /**
   * Focuses the component.
   *
   * @return the component itself.
   */
  public HasFocus focus();
}
