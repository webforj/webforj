package com.webforj.concern;

/**
 * An interface for components that accept files dropped by the user.
 *
 * @param <T> the type of the component that implements this interface
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public interface HasFileDrop<T> {

  /**
   * Enables or disables the drop zone.
   *
   * @param drop {@code true} to enable, {@code false} to disable
   * @return the component itself
   */
  T setDrop(boolean drop);

  /**
   * Checks whether the drop zone is enabled.
   *
   * @return {@code true} if enabled, {@code false} otherwise
   */
  boolean isDrop();
}
