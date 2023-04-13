package org.dwcj.component.button.event;

import java.util.Map;
import org.dwcj.component.button.Button;
import org.dwcj.component.events.Event;

/**
 * A push, or click, event for the Button class.
 */
public class ButtonClickEvent extends Event<Button> {

  /**
   * Creates a new event.
   *
   * @param control the control
   * @param eventMap the event map
   */
  public ButtonClickEvent(Button control, Map<String, Object> eventMap) {
    super(control, eventMap);
  }
}
