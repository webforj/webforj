package org.dwcj.component.list.event;

import java.util.Map;
import org.dwcj.component.list.DwcSelectDropdown;

/**
 * An event which is fired when one of the list items is clicked.
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
