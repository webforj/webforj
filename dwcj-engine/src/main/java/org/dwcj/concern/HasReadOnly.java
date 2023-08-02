package org.dwcj.concern;

/**
 * Facilitates implementation of methods which allow for the toggle of read only status on a
 * control.
 */
public interface HasReadOnly {

  /**
   * Sets whether a user can edit the control.
   *
   * @param editable True to disable editing, false to enable editing.
   * @return The control itself.
   */
  HasReadOnly setReadOnly(Boolean editable);

  /**
   * Returns a value indicating whether or not a control is set to read only or not.
   *
   * @return Boolean indicating whether or not the user can edit the control.
   */
  Boolean isReadOnly();

}
