package com.webforj.data.concern;

/**
 * An interface for components that pick a selection mode such as files, directories, or both.
 *
 * <p>
 * The mode type {@code M} is component specific. Each component picks its own enum so call sites
 * read naturally with the component name in the import path.
 * </p>
 *
 * @param <T> the type of the component that implements this interface
 * @param <M> the type of the selection mode value
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public interface HasSelectionMode<T, M> {

  /**
   * Sets the selection mode.
   *
   * @param selectionMode the selection mode
   * @return the component itself
   */
  T setSelectionMode(M selectionMode);

  /**
   * Gets the selection mode.
   *
   * @return the selection mode
   */
  M getSelectionMode();
}
