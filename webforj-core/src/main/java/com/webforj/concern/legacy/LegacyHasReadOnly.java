package com.webforj.concern.legacy;

/**
 * Facilitates implementation of methods which allow for the toggle of read only status on a
 * control.
 *
 * @deprecated Use {@link HasReadOnly} instead.
 */
@Deprecated(since = "23.05", forRemoval = true)
public interface LegacyHasReadOnly {

  /**
   * Sets whether a user can edit the control.
   *
   * @param editable True to disable editing, false to enable editing.
   * @return The control itself.
   */
  LegacyHasReadOnly setReadOnly(Boolean editable);

  /**
   * Returns a value indicating whether or not a control is set to read only or not.
   *
   * @return Boolean indicating whether or not the user can edit the control.
   */
  Boolean isReadOnly();

}
