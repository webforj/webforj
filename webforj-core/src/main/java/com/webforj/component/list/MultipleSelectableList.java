package com.webforj.component.list;

import com.webforj.component.Component;
import com.webforj.data.selection.MultipleSelectable;

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
public interface MultipleSelectableList<T extends Component>
    extends MultipleSelectable<T, ListItem>, SelectableList<T> {

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
   * Returns the selection mode of the list box.
   *
   * @return the selection mode
   */
  public SelectionMode getSelectionMode();
}

