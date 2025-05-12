package com.webforj.data.selection;

/**
 * Allows selecting and deselecting a single item by its reference.
 *
 * @param <T> the component type.
 * @param <V> the type of the selected item
 *
 * @author Hyyan Abo Fakher
 * @since 25.01
 */
public interface ItemSingleSelectable<T, V> extends ItemSingleSelectionAware<V> {
  /**
   * Selects the given item.
   *
   * @param item the item to select
   * @return the component itself
   */
  T select(V item);
}
