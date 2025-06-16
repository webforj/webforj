package com.webforj.data.selection;

import java.util.List;

/**
 * Provides read-only access to selected keys in a multi-selection context.
 *
 * @author Hyyan Abo Fakher
 * @since 25.01
 */
public interface KeyMultiSelectionAware {

  /**
   * Returns the list of selected keys.
   *
   * @return the list of selected keys
   */
  List<Object> getSelectedKeys();
}
