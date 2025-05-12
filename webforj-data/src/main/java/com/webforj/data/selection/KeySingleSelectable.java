package com.webforj.data.selection;

/**
 * Allows selection of a single item by its key.
 *
 * @param <T> the component type.
 *
 * @author Hyyan Abo Fakher
 * @since 25.01
 */
public interface KeySingleSelectable<T> extends KeySingleSelectionAware {

  /**
   * Selects the item with the given key.
   *
   * @param key the key of the item to select
   * @return the component itself
   */
  T selectKey(Object key);

  /**
   * Deselects the selected item.
   *
   * @return the component itself
   */
  T deselect();
}
