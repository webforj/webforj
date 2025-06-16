package com.webforj.data.selection;

/**
 * Allows selection of multiple items by their keys.
 *
 * @param <T> the component type.
 *
 * @author Hyyan Abo Fakher
 * @since 25.01
 */
public interface KeyMultiSelectable<T> extends KeyMultiSelectionAware {
  /**
   * Selects multiple items with the given keys.
   *
   * @param keys the keys of the items to select
   * @return the component itself
   */
  T selectKey(Object... keys);

  /**
   * Deselects the item with the given key.
   *
   * @param key the key of the item to deselect
   * @return the component itself
   */
  T deselectKey(Object... key);

  /**
   * Deselects all items in the list.
   *
   * @return the component itself
   */
  T deselectAll();
}
