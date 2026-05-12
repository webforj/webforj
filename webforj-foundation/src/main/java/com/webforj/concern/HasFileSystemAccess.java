package com.webforj.concern;

/**
 * An interface for components that opt into the browser File System Access API for file picking.
 *
 * @param <T> the type of the component that implements this interface
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public interface HasFileSystemAccess<T> {

  /**
   * Sets whether the component is allowed to use the browser File System Access API.
   *
   * @param fileSystemAccess {@code true} to allow, {@code false} to deny
   * @return the component itself
   */
  T setFileSystemAccess(boolean fileSystemAccess);

  /**
   * Checks whether the component may use the File System Access API.
   *
   * @return {@code true} if allowed, {@code false} otherwise
   */
  boolean isFileSystemAccess();
}
