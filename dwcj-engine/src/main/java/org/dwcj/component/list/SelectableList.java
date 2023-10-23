package org.dwcj.component.list;

import org.dwcj.component.Component;

/**
 * An interface for lists that support single item selection.
 *
 * <p>
 * This interface provides methods and properties for selecting and retrieving the currently
 * selected item within a list.
 * </p>
 *
 * @param <T> the type of the component
 *
 * @see MultipleSelectableList
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public interface SelectableList<T extends Component> {

  /**
   * Deselects the first selected item.
   *
   * @return the component itself
   */
  public T deselect();

  /**
   * Selects the given item.
   *
   * @param item the item to select
   * @return the component itself
   *
   * @throws IllegalArgumentException if the given item is not in the list
   */
  public T select(ListItem item);

  /**
   * Selects the item with the given key.
   *
   * @param key the key of the item to select
   * @return the component itself
   *
   * @throws IllegalArgumentException if the given item is not in the list
   */
  public T selectKey(Object key);

  /**
   * Selects the item at the given index.
   *
   * @param index the index of the item to select
   * @return the component itself
   *
   * @throws IndexOutOfBoundsException if the given index is out of bounds
   */
  public T selectIndex(int index);

  /**
   * Returns the first selected item.
   *
   * @return the selected item or null if there is no selected item
   */
  public ListItem getSelected();

  /**
   * Alias for {@link #getSelected()}.
   *
   * @return the selected item
   * @see #getSelected()
   */
  public default ListItem getSelectedItem() {
    return getSelected();
  }

  /**
   * Returns the key of the first selected item.
   *
   * @return the key of the selected item or null if there is no selected item
   */
  public Object getSelectedKey();

  /**
   * Returns the index of the first selected item.
   *
   * @return the index of the selected item or -1 if there is no selected item
   */
  public int getSelectedIndex();
}
