package com.webforj.data.selection.event;

import java.util.List;

/**
 * An interface for select events that are emitted when an item inside a component is selected.
 *
 * @param <T> the type of the component
 * @param <V> the type of the selected item
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public interface SelectEvent<V> {

  /**
   * Gets the selected index.
   *
   * @return the selected index or -1 if no item is selected
   */
  int getSelectedIndex();

  /**
   * Gets the selected item.
   *
   * @return the selected item
   */
  V getSelectedItem();

  /**
   * Gets the selected indices.
   *
   * <p>
   * If the list does not support multiple selection, then the returned list will contain only one
   * item.
   * </p>
   *
   * @return the selected indices or an empty list if no item is selected
   */
  List<Integer> getSelectedIndices();

  /**
   * Gets the selected items.
   *
   * <p>
   * If the list does not support multiple selection, then the returned list will contain only one
   * item.
   * </p>
   *
   * @return the selected items
   */
  List<V> getSelectedItems();
}
