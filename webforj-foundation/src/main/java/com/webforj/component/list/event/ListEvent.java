package com.webforj.component.list.event;

import com.webforj.component.event.AbstractSelectEvent;
import com.webforj.component.list.DwcList;
import com.webforj.component.list.ListItem;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * The base class for all list events.
 *
 * @param <V> the type of the list items
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public class ListEvent<V> extends AbstractSelectEvent<DwcList<?, V>, ListItem> {

  protected ListEvent(DwcList<?, V> component, Map<String, Object> eventMap) {
    super(component, eventMap);
  }

  /**
   * Gets the selected item.
   *
   * @return the selected item
   */
  public ListItem getSelectedItem() {
    int index = getSelectedIndex();
    if (index == -1) {
      return null;
    }

    DwcList<?, V> component = (DwcList<?, V>) getComponent();
    return component.getByIndex(index);
  }

  /**
   * Gets the selected items.
   *
   * <p>
   * If the list does not support multiple selection, then the returned list will contain only one
   * item.
   * </p>
   *
   * @return the selected items
   */
  public List<ListItem> getSelectedItems() {
    List<Integer> indices = getSelectedIndices();
    DwcList<?, V> component = (DwcList<?, V>) getComponent();

    return indices.stream().map(component::getByIndex).filter(Objects::nonNull).toList();
  }
}
