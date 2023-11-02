package org.dwcj.component.event;

import java.util.Map;
import org.dwcj.component.Component;

/**
 * An event that is fired when the mouse cursor exits the boundaries of an element. 
 * It occurs when the user moves the mouse pointer out of the boundaries of the specified 
 * element, indicating that the mouse has exited its area.
 *
 * @see MouseEnterEvent
 * {@link MouseEvent}
 */
public class MouseExitEvent extends MouseEvent {

  public MouseExitEvent(Component component, Map<String, Object> payload) {
    super(component, payload);
  }
}
