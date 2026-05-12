package com.webforj.concern;

/**
 * An interface for file chooser components that allow the user to select multiple filters at once.
 *
 * @param <T> the type of the component that implements this interface
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public interface HasFileChooserMultiFilterSelection<T> {

  /**
   * Sets whether the user may select multiple filters at once.
   *
   * <p>
   * Only effective inside the custom filter menu rendered by the component. When the File System
   * Access API is enabled and the browser supports it, the native operating system file picker
   * dialog accepts only one filter at a time, and this setting is ignored in that mode.
   * </p>
   *
   * @param multiFilterSelection {@code true} to allow, {@code false} to deny
   * @return the component itself
   */
  T setMultiFilterSelection(boolean multiFilterSelection);

  /**
   * Checks whether multiple filter selection is enabled.
   *
   * <p>
   * Returns the configured value. The File System Access API may override this at runtime when the
   * native picker is in use.
   * </p>
   *
   * @return {@code true} if enabled, {@code false} otherwise
   */
  boolean isMultiFilterSelection();
}
