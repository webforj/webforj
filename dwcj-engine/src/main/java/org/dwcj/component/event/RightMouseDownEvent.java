package org.dwcj.component.event;

import java.util.Map;
import org.dwcj.component.Component;

/**
 * An event that is fired when the right mouse button is pressed.
 *
 * {@link MouseEvent}
 */
public class RightMouseDownEvent extends MouseEvent {

  public RightMouseDownEvent(Component component, Map<String, Object> payload) {
    super(component, payload);
  }
}
