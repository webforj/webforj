package org.dwcj.component.list.event;

import java.util.Map;
import org.dwcj.component.list.DwcSelectDropdown;

/**
 * An event which is fired when a list dropdown is closed.
 *
 * @see ListOpenEvent
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public class ListCloseEvent extends ListEvent {

  /**
   * Creates a new event.
   *
   * @param component the component
   * @param eventMap the event map
   */
  public ListCloseEvent(DwcSelectDropdown<?> component, Map<String, Object> eventMap) {
    super(component, eventMap);
  }
}
