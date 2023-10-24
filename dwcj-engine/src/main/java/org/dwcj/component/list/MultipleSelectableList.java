package org.dwcj.component.list;

import java.util.List;
import org.dwcj.component.Component;

/**
 * An interface for lists that support multiple selection.
 *
 * <p>
 * This interface extends the {@link SelectableList} interface and introduces methods and properties
 * specific to managing multiple selections within a list.
 * </p>
 *
 * @param <T> the type of the component.
 *
 * @see SelectableList
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public interface MultipleSelectableList<T extends Component> extends SelectableList<T> {

  /**
   * The selection mode of the list box.
   */
  public enum SelectionMode {
    /**
     * The user can select only one item at a time.
     */
    SINGLE,
    /**
     * The user can select multiple items at a time.
     */
    MULTIPLE
  }

  /**
   * Sets the selection mode of the list box.
   *
   * @param mode the selection mode
   * @return the component itself
   */
  public T setSelectionMode(SelectionMode mode);

  /**
   * Deselects the first selected item.
   *
   * @return the component itself
   * @throws IllegalArgumentException if the given item is not in the list
   */
  @Override
  public T deselect();

  /**
   * Returns the selection mode of the list box.
   *
   * @return the selection mode
   */
  public SelectionMode getSelectionMode();

  /**
   * Deselects the given item.
   *
   * @param item the item to deselect
   *
   * @return the component itself
   *
   * @throws IllegalArgumentException if the given item is not in the list
   * @throws IllegalStateException if the list is not in multiple selection mode
   */
  public T deselect(ListItem item);

  /**
   * Deselects the item with the given key.
   *
   * @param key the key of the item to deselect
   * @return the component itself
   *
   * @throws IllegalStateException if the list is not in multiple selection mode
   * @throws IllegalArgumentException if the given item is not in the list
   */
  public T deselectKey(Object key);

  /**
   * Deselects the item at the given index.
   *
   * @param index the index of the item to deselect
   * @return the component itself
   *
   * @throws IndexOutOfBoundsException if the given index is out of bounds
   * @throws IllegalStateException if the list is not in multiple selection mode
   * @throws IllegalArgumentException if the given item is not in the list
   */
  public T deselectIndex(int index);

  /**
   * Deselects all items in the list.
   *
   * @return the component itself
   */
  public T deselectAll();

  /**
   * Selects multiple items in the list.
   *
   * @param items the items to select
   * @return the component itself
   *
   * @throws IllegalArgumentException if the given items are not in the list
   * @throws IllegalStateException if the list is not in multiple selection mode
   */
  public T select(ListItem... items);

  /**
   * Selects multiple items with the given keys.
   *
   * @param keys the keys of the items to select
   * @return the component itself
   *
   * @throws IllegalArgumentException if the given items are not in the list
   * @throws IllegalStateException if the list is not in multiple selection mode
   */
  public T selectKeys(Object... keys);

  /**
   * Selects multiple items with the given indices.
   *
   * @param indices the indexes of the items to select
   * @return the component itself
   *
   * @throws IllegalArgumentException if the given items are not in the list
   * @throws IllegalStateException if the list is not in multiple selection mode
   */
  public T selectIndices(int... indices);

  /**
   * Returns the list of selected items.
   *
   * @return the list of selected items
   */
  public List<ListItem> getSelectedItems();

  /**
   * Returns the list of selected keys.
   *
   * @return the list of selected keys
   */
  public List<Object> getSelectedKeys();

  /**
   * Returns the list of selected indices.
   *
   * @return the list of selected indices
   */
  public List<Integer> getSelectedIndices();
}
