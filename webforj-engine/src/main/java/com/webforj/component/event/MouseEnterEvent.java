package com.webforj.component.event;

import com.webforj.component.Component;
import java.util.Map;

/**
 * An event that is fired when the mouse cursor enters the boundaries of an element.
 *
 * <p>
 * It occurs when the user moves the mouse pointer over the specified element, indicating that the
 * mouse has entered its area.
 * </p>
 *
 * @see MouseExitEvent {@link MouseEvent}
 */
public class MouseEnterEvent extends MouseEvent {

  public MouseEnterEvent(Component component, Map<String, Object> payload) {
    super(component, payload);
  }
}
