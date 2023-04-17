package org.dwcj.component.field.event;

import java.util.Map;
import org.dwcj.component.events.Event;
import org.dwcj.component.field.Field;

/**
 * A key press event for the Field class.
 */
public class FieldEditKeypressEvent extends Event<Field> {

  /**
   * Creates a new event.
   *
   * @param control the control
   * @param eventMap the event map
   */
  public FieldEditKeypressEvent(Field control, Map<String, Object> eventMap) {
    super(control, eventMap);
  }
}
