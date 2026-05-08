package com.webforj.concern;

/**
 * An interface for components that constrain the maximum size of an uploaded or selected file.
 *
 * @param <T> the type of the component that implements this interface
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public interface HasMaxFileSize<T> {

  /**
   * Sets the maximum allowed size of an uploaded file in bytes.
   *
   * @param maxFileSize the maximum size in bytes
   * @return the component itself
   */
  T setMaxFileSize(Number maxFileSize);

  /**
   * Gets the maximum file size.
   *
   * @return the maximum file size in bytes
   */
  Number getMaxFileSize();
}
