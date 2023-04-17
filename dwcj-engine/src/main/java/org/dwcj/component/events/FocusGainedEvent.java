package org.dwcj.component.events;

import java.util.Map;
import org.dwcj.component.AbstractDwcComponent;

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
