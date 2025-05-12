package com.webforj.data.selection;

/**
 * Provides read-only access to selected item in a single-selection context.
 *
 * @param <V> the type of the selected item
 *
 * @author Hyyan Abo Fakher
 * @since 25.01
 */
public interface ItemSingleSelectionAware<V> {

  /**
   * Returns the selected item.
   *
   * @return the selected item or null if there is no selected item
   */
  V getSelected();

  /**
   * Alias for {@link #getSelected()}.
   *
   * @return the selected item
   * @see #getSelected()
   */
  default V getSelectedItem() {
    return getSelected();
  }
}
