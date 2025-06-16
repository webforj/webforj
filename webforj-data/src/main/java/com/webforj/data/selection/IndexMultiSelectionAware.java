package com.webforj.data.selection;

import java.util.List;

/**
 * Provides read-only access to selected indices in a multi-selection context.
 *
 * @author Hyyan Abo Fakher
 * @since 25.01
 */
public interface IndexMultiSelectionAware {

  /**
   * Returns the list of selected item indices.
   *
   * @return a list of selected indices, or an empty list if no selection exists
   */
  List<Integer> getSelectedIndices();
}
