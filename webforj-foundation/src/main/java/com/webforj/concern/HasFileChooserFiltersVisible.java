package com.webforj.concern;

/**
 * An interface for file chooser components that expose the visibility of the filter selector.
 *
 * @param <T> the type of the component that implements this interface
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public interface HasFileChooserFiltersVisible<T> {

  /**
   * Sets whether the filter selector is visible.
   *
   * <p>
   * Only effective when the component renders its own filter menu. When the File System Access API
   * is enabled and the browser supports it, the native operating system file picker dialog manages
   * filter selection itself, and this setting is ignored. Disable the File System Access API to
   * make the toggle take effect.
   * </p>
   *
   * @param filtersVisible {@code true} to show, {@code false} to hide
   * @return the component itself
   */
  T setFiltersVisible(boolean filtersVisible);

  /**
   * Checks whether the filter selector is visible.
   *
   * <p>
   * Returns the configured value. The File System Access API may override this at runtime so the
   * configured value can differ from what the user sees in the native picker.
   * </p>
   *
   * @return {@code true} if visible, {@code false} otherwise
   */
  boolean isFiltersVisible();
}
