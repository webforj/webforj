package com.webforj.data.selection;

/**
 * Allows selecting and deselecting multiple items by their indices.
 *
 * @param <T> the component type.
 *
 * @author Hyyan Abo Fakher
 * @since 25.01
 */
public interface IndexMultiSelectable<T> extends IndexMultiSelectionAware {
  /**
   * Selects multiple items with the given indices.
   *
   * @param indices the indexes of the items to select
   * @return the component itself
   */
  T selectIndex(int... indices);

  /**
   * Deselects the item at the given index.
   *
   * @param index the index of the item to deselect
   * @return the component itself
   */
  T deselectIndex(int... index);

  /**
   * Deselects all items in the list.
   *
   * @return the component itself
   */
  T deselectAll();
}
