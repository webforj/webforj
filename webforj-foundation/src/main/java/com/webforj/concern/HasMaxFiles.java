package com.webforj.concern;

/**
 * An interface for components that constrain the maximum number of files that can be uploaded or
 * selected at once.
 *
 * @param <T> the type of the component that implements this interface
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public interface HasMaxFiles<T> {

  /**
   * Sets the maximum number of files that can be uploaded or selected.
   *
   * @param maxFiles the maximum number of files
   * @return the component itself
   */
  T setMaxFiles(Number maxFiles);

  /**
   * Gets the maximum number of files that can be uploaded or selected.
   *
   * @return the maximum number of files
   */
  Number getMaxFiles();
}
