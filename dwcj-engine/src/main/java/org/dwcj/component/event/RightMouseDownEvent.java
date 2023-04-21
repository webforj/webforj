package org.dwcj.component.event;

import java.util.Map;
import org.dwcj.component.AbstractComponent;

/**
 * An event that is fired when the right mousebutton is pressed.
 *
 * {@link MouseEvent}
 */
public class RightMouseDownEvent extends MouseEvent {

  public RightMouseDownEvent(AbstractComponent component, Map<String, Object> payload) {
    super(component, payload);
  }
}
