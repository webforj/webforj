package com.webforj.component.event;

import com.webforj.component.Component;
import java.util.Map;

/**
 * An event which is fired when the user clicks the right mouse button while in the bounds of a the
 * component.
 *
 * {@link MouseEvent}
 */
public class RightMouseDownEvent extends MouseEvent {

  public RightMouseDownEvent(Component component, Map<String, Object> payload) {
    super(component, payload);
  }
}
