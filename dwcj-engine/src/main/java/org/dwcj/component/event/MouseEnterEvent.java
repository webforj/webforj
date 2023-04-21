package org.dwcj.component.event;

import java.util.Map;
import org.dwcj.component.AbstractComponent;

/**
 * An event that is fired when the mouse enters a component.
 *
 * {@link MouseEvent}
 */
public class MouseEnterEvent extends MouseEvent {

  public MouseEnterEvent(AbstractComponent component, Map<String, Object> payload) {
    super(component, payload);
  }
}
