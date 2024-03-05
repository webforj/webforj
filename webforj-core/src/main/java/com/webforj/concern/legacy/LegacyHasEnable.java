package com.webforj.concern.legacy;

/**
 * Interface that facilitates implementation of behavior which determines whether or not a control
 * can be disabled after being rendered to the page.
 *
 * @deprecated Use {@link HasEnable} instead.
 */
@Deprecated(since = "23.05", forRemoval = true)
public interface LegacyHasEnable {

  /**
   * Returns whether the control is enabled.
   *
   * @return true if the control is enabled, false otherwise
   */
  public boolean isEnabled();

  /**
   * Set whether the control is to be enabled.
   *
   * @param enabled true if the control is to be enabled, false otherwise
   * @return the control itself
   */
  public LegacyHasEnable setEnabled(boolean enabled);
}
