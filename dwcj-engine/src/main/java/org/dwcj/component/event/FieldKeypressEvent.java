package org.dwcj.component.event;

import java.util.Map;
import org.dwcj.component.AbstractComponent;

/**
 * An event that is fired when a key is pressed in a field component.
 *
 * {@link KeypressEvent}
 */
public class FieldKeypressEvent extends KeypressEvent {

  public FieldKeypressEvent(AbstractComponent component, Map<String, Object> payload) {
    super(component, payload);
  }
}
