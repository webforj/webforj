package com.webforj.component.list.event;

import com.webforj.component.list.DwcList;
import com.webforj.component.list.ListItem;
import com.webforj.data.selection.event.AbstractSelectEvent;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * The base class for all list events.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public class ListEvent extends AbstractSelectEvent<DwcList<?>, ListItem> {

  protected ListEvent(DwcList<?> component, Map<String, Object> eventMap) {
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

    DwcList<?> component = (DwcList<?>) getComponent();
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
    DwcList<?> component = (DwcList<?>) getComponent();

    return indices.stream().map(component::getByIndex).filter(Objects::nonNull).toList();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public DwcList<?> getComponent() {
    return (DwcList<?>) super.getComponent();
  }
}
