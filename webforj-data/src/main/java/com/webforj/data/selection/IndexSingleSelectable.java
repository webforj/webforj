package com.webforj.data.selection;

/**
 * Allows selecting and deselecting a single item by its index.
 *
 * @param <T> the component type.
 *
 * @author Hyyan Abo Fakher
 * @since 25.01
 */
public interface IndexSingleSelectable<T> extends IndexSingleSelectionAware {
  /**
   * Selects the item at the given index.
   *
   * @param index the index of the item to select
   * @return the component itself
   */
  T selectIndex(int index);

  /**
   * Deselects the selected item.
   *
   * @return the component itself
   */
  T deselect();
}
