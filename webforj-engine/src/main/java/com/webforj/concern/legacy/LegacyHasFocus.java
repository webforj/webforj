package com.webforj.concern.legacy;

/**
 * Interface that facilitates allowing a component to receive focus on the page.
 *
 * @deprecated Use {@link HasFocus} instead.
 */
@Deprecated(since = "23.05", forRemoval = true)
public interface LegacyHasFocus {

  /**
   * Gives a component focus when it is added to the window. Note that if this method is called on
   * multiple components, focus will be given to the component added latest to the window.
   *
   * @return The component itself
   */
  public LegacyHasFocus focus();
}
