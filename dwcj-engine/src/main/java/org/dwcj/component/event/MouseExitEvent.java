package org.dwcj.component.event;

import java.util.Map;
import org.dwcj.component.AbstractComponent;

/**
 * An event that is fired when the mouse exits a component.
 *
 * {@link MouseEvent}
 */
public class MouseExitEvent extends MouseEvent {

  public MouseExitEvent(AbstractComponent component, Map<String, Object> payload) {
    super(component, payload);
  }
}
