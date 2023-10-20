package org.dwcj.component.list.event;

import java.util.Map;
import org.dwcj.component.list.DwcSelectDropdown;

/**
 * An event which is fired when a list dropdown is opened.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public class ListOpenEvent extends ListEvent {

  /**
   * Creates a new event.
   *
   * @param component the component
   * @param eventMap the event map
   */
  public ListOpenEvent(DwcSelectDropdown<?> component, Map<String, Object> eventMap) {
    super(component, eventMap);
  }
}
