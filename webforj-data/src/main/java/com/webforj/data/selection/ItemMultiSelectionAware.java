package com.webforj.data.selection;

import java.util.List;

/**
 * Provides read-only access to selected item in a multi-selection context.
 *
 * @param <V> the type of the selected item
 *
 * @author Hyyan Abo Fakher
 * @since 25.01
 */
public interface ItemMultiSelectionAware<V> {
  /**
   * Returns the list of selected items.
   *
   * @return the list of selected items
   */
  List<V> getSelectedItems();

  /**
   * Returns whether the given item is selected.
   *
   * @param item the item to check
   * @return {@code true} if the given item is selected, {@code false} otherwise
   */
  default boolean isSelected(V item) {
    return getSelectedItems().contains(item);
  }
}
