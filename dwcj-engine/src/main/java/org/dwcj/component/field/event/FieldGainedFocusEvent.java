package org.dwcj.component.field.event;

import java.util.Map;
import org.dwcj.component.events.Event;
import org.dwcj.component.field.Field;

/**
 * A gained focus event for the Field class.
 */
public class FieldGainedFocusEvent extends Event<Field> {

  /**
   * Creates a new event.
   *
   * @param control the control
   * @param eventMap the event map
   */
  public FieldGainedFocusEvent(Field control, Map<String, Object> eventMap) {
    super(control, eventMap);
  }
}
