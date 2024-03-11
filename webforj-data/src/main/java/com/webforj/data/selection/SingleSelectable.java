package com.webforj.data.selection;

/**
 * An interface for components that support single item selection.
 *
 * <p>
 * This interface provides methods and properties for selecting and retrieving the currently
 * selected item within a component.
 * </p>
 *
 * @param <T> the type of the component
 * @param <V> the type of the selected item
 *
 * @see MultipleSelectable
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public interface SingleSelectable<T, V> extends Selectable<V> {

  /**
   * Selects the given item.
   *
   * @param item the item to select
   * @return the component itself
   */
  T select(V item);

  /**
   * Selects the item with the given key.
   *
   * @param key the key of the item to select
   * @return the component itself
   */
  T selectKey(Object key);

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
