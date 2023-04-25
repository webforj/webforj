package org.dwcj.component.event;

import java.util.Map;
import org.dwcj.component.AbstractComponent;

/**
 * An event that is fired when a key is pressed in a edit component.
 *
 * {@link KeypressEvent}
 */
public class EditKeypressEvent extends KeypressEvent {

  public EditKeypressEvent(AbstractComponent component, Map<String, Object> payload) {
    super(component, payload);
  }
}
