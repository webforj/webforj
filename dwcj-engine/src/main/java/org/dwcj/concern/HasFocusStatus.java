package org.dwcj.concern;

/**
 * An interface that enables components to check their focus status on the page.
 *
 * <p>
 * This interface provides a method to check whether the component has focus. Note that this method
 * may reach the client to determine the focus state. If the component is not attached to a panel,
 * it will return false.
 * </p>
 *
 * @see HasFocus
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public interface HasFocusStatus {

  /**
   * Checks if the component has focus.
   *
   * @return true if the component has focus, false if not.
   */
  public boolean hasFocus();
}
