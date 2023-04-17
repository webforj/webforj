package org.dwcj.component.events.shared.events;

import java.util.Map;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.events.Event;

/**
 * A gained focus event for the Field class.
 */
public class FocusGainedEvent extends Event<AbstractDwcComponent> {

  /**
   * Creates a new event.
   *
   * @param control the control
   * @param eventMap the event map
   */
  public FocusGainedEvent(AbstractDwcComponent control, Map<String, Object> eventMap) {
    super(control, eventMap);
  }
}
