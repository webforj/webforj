package org.dwcj.component.field.event;

import java.util.Map;
import org.dwcj.component.events.Event;
import org.dwcj.component.field.Field;

/**
 * A lost focus event for the Field class.
 */
public class FieldLostFocusEvent extends Event<Field> {

  /**
   * Creates a new event.
   *
   * @param control the control
   * @param eventMap the event map
   */
  public FieldLostFocusEvent(Field control, Map<String, Object> eventMap) {
    super(control, eventMap);
  }
}
