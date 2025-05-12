package com.webforj.data.selection;

/**
 * Provides read-only access to the selected key in a single-selection context.
 *
 * @author Hyyan Abo Fakher
 * @since 25.01
 */
public interface KeySingleSelectionAware {
  /**
   * Returns the key of the selected item.
   *
   * @return the key of the selected item or null if there is no selected item
   */
  Object getSelectedKey();
}
