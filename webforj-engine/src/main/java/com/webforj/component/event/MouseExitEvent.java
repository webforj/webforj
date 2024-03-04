package com.webforj.component.event;

import com.webforj.component.Component;
import java.util.Map;

/**
 * An event that is fired when the mouse cursor exits the boundaries of an element.
 *
 * <p>
 * It occurs when the user moves the mouse pointer out of the boundaries of the specified element,
 * indicating that the mouse has exited its area.
 * </p>
 *
 * @see MouseEnterEvent {@link MouseEvent}
 */
public class MouseExitEvent extends MouseEvent {

  public MouseExitEvent(Component component, Map<String, Object> payload) {
    super(component, payload);
  }
}
