package org.dwcj.component.list.event;

import java.util.Map;
import org.dwcj.component.list.DwcList;

/**
 * An event which is fired when the user selects an item from a list.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public class ListSelectEvent extends ListEvent {

  /**
   * Creates a new event.
   *
   * @param component the component
   * @param eventMap the event map
   */
  public ListSelectEvent(DwcList<?> component, Map<String, Object> eventMap) {
    super(component, eventMap);
  }
}
