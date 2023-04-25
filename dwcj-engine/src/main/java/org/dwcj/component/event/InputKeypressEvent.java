package org.dwcj.component.event;

import java.util.Map;
import org.dwcj.component.AbstractComponent;

/**
 * An event that is fired when a key is pressed in a input component.
 *
 * {@link KeypressEvent}
 */
public class InputKeypressEvent extends KeypressEvent {

  public InputKeypressEvent(AbstractComponent component, Map<String, Object> payload) {
    super(component, payload);
  }
}
