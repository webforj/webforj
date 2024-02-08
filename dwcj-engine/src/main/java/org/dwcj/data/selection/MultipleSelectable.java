package org.dwcj.data.selection;

import java.util.List;
import org.dwcj.component.Component;

/**
 * An interface for components that support multiple selection.
 *
 * <p>
 * This interface introduces methods and properties specific to managing multiple selections within
 * a component.
 * </p>
 *
 * @param <T> the type of the component.
 * @param <V> the type of the selected item
 *
 * @see SingleSelectable
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public interface MultipleSelectable<T extends Component, V> extends Selectable<V> {

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
   * Selects multiple items with the given keys.
   *
   * @param keys the keys of the items to select
   * @return the component itself
   */
  T selectKey(Object... keys);

  /**
   * Selects multiple items with the given indices.
   *
   * @param indices the indexes of the items to select
   * @return the component itself
   */
  T selectIndex(int... indices);

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

  /**
   * Deselects the item with the given key.
   *
   * @param key the key of the item to deselect
   * @return the component itself
   */
  T deselectKey(Object... key);

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

  /**
   * Returns the list of selected items.
   *
   * @return the list of selected items
   */
  List<V> getSelectedItems();

  /**
   * Returns the list of selected keys.
   *
   * @return the list of selected keys
   */
  List<Object> getSelectedKeys();

  /**
   * Returns the list of selected indices.
   *
   * @return the list of selected indices
   */
  List<Integer> getSelectedIndices();

  /**
   * Returns whether the given item is selected.
   *
   * @param item the item to check
   * @return {@code true} if the given item is selected, {@code false} otherwise
   */
  default boolean isSelected(V item) {
    return getSelectedItems().contains(item);
  }
}
