package org.dwcj.component.event;

import java.util.Map;
import org.dwcj.component.Component;

/**
 * An event that is fired when the mouse enters a component.
 *
 * {@link MouseEvent}
 */
public class MouseEnterEvent extends MouseEvent {

  public MouseEnterEvent(Component component, Map<String, Object> payload) {
    super(component, payload);
  }
}
