package com.webforj.concern.legacy;

/**
 * Interface facilitating the implementation of behavior to access and mutate a control's tab
 * traversal on a rendered page.
 *
 * @deprecated Use {@link HasFocus} instead.
 */
@Deprecated(since = "23.05", forRemoval = true)
public interface LegacyHasTabTraversal {

  /**
   * Returns a boolean indicating whether or not the user can navigate to the control using the tab
   * key.
   *
   * @return True if user can navigate to control with Tab key, False if not
   */
  Boolean isTabTraversable();

  /**
   * Sets whether or not the user can navigate to the control using the tab key.
   *
   * @param traversable Boolean dictating tab traversal. True if control can be navigated to with
   *        Tab, False if not.
   * @return The control itself
   */
  LegacyHasTabTraversal setTabTraversable(Boolean traversable);
}
