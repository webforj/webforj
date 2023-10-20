package org.dwcj.component.list.event;

import com.basis.startup.type.BBjVector;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;
import org.dwcj.component.event.ComponentEvent;
import org.dwcj.component.list.DwcList;
import org.dwcj.component.list.ListItem;

/**
 * The base class for all list events.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public class ListEvent extends ComponentEvent<DwcList<?>> {

  protected ListEvent(DwcList<?> component, Map<String, Object> eventMap) {
    super(component, eventMap);
  }

  /**
   * Gets the selected index.
   *
   * @return the selected index or -1 if no item is selected
   */
  public int getSelectedIndex() {
    Object selectedIndex = getEventMap().get("index");
    if (selectedIndex == null) {
      return -1;
    }

    return Integer.parseInt(String.valueOf(selectedIndex));
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
   * Gets the selected indices.
   *
   * <p>
   * If the list does not support multiple selection, then the returned list will contain only one
   * item.
   * </p>
   *
   * @return the selected indices or an empty list if no item is selected
   */
  public List<Integer> getSelectedIndices() {
    Object selectedIndices = getEventMap().get("indices");
    if (selectedIndices == null) {
      return Collections.emptyList();
    }

    BBjVector indices = (BBjVector) selectedIndices;
    return Arrays.stream(indices.toArray())
        .mapToInt(index -> Integer.parseInt(String.valueOf(index))).boxed().toList();
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
}
