package org.dwcj.data.selection;

/**
 * The base interface for components that support item selection.
 *
 * @param <V> the type of the selected item
 *
 * @see SingleSelectable
 * @see MultipleSelectable
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public interface Selectable<V> {

  /**
   * Returns the selected item.
   *
   * @return the selected item or null if there is no selected item
   */
  V getSelected();

  /**
   * Alias for {@link #getSelected()}.
   *
   * @return the selected item
   * @see #getSelected()
   */
  default V getSelectedItem() {
    return getSelected();
  }

  /**
   * Returns the key of the selected item.
   *
   * @return the key of the selected item or null if there is no selected item
   */
  Object getSelectedKey();

  /**
   * Returns the index of the first selected item.
   *
   * @return the index of the selected item or -1 if there is no selected item
   */
  int getSelectedIndex();
}
