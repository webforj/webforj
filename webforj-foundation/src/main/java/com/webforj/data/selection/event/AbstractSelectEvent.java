package com.webforj.data.selection.event;

import com.basis.startup.type.BBjVector;
import com.webforj.component.Component;
import com.webforj.component.event.ComponentEvent;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * An abstract select event that emitted when an item inside a component is selected.
 *
 * @param <T> the type of the component
 * @param <V> the type of the selected item
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public abstract class AbstractSelectEvent<T extends Component, V> extends ComponentEvent<T>
    implements SelectEvent<V> {

  protected AbstractSelectEvent(T component, Map<String, Object> eventMap) {
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
}
