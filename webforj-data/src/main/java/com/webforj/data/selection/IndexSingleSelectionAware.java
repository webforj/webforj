package com.webforj.data.selection;

/**
 * Provides read-only access to the selected index in a single-selection context.
 *
 * @author Hyyan Abo Fakher
 * @since 25.01
 */
public interface IndexSingleSelectionAware {

  /**
   * Returns the index of the selected item.
   *
   * @return the index of the selected item, or {@code -1} if no item is selected
   */
  int getSelectedIndex();
}
