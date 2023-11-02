package org.dwcj.component.list.event;

import java.util.Map;
import org.dwcj.component.list.DwcSelectDropdown;

/**
 * This event is triggered when the user clicks an item from a List-based component. It provides
 * essential information about the selected item and allows developers to implement custom 
 * actions or responses when an item is chosen.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public class ListClickEvent extends ListEvent {

  /**
   * Creates a new event.
   *
   * @param component the component
   * @param eventMap the event map
   */
  public ListClickEvent(DwcSelectDropdown<?> component, Map<String, Object> eventMap) {
    super(component, eventMap);
  }
}
