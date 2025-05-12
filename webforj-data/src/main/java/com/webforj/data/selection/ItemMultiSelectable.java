package com.webforj.data.selection;

/**
 * Allows selecting and deselecting multiple items by their references.
 *
 * @param <T> the component type.
 * @param <V> the type of the selected item
 *
 * @author Hyyan Abo Fakher
 * @since 25.01
 */
public interface ItemMultiSelectable<T, V> extends ItemMultiSelectionAware<V> {
  /**
   * Selects multiple items in the list.
   *
   * <p>
   * <strong>Note:</strong> Components implanting this method should ensure type safety for
   * potential heap pollution via varargs parameter.
   * </p>
   *
   * @param items the items to select
   * @return the component itself
   */
  T select(V... items);

  /**
   * Deselects the given item.
   *
   * <p>
   * <strong>Note:</strong> Components implanting this method should ensure type safety for
   * potential heap pollution via varargs parameter.
   * </p>
   *
   * @param items the items to deselect
   *
   * @return the component itself
   */
  T deselect(V... items);
}
