package org.dwcj.component.event;

import java.util.Map;
import org.dwcj.component.AbstractComponent;

/**
 * An event that is fired when a component is checked on.
 */
public class CheckedEvent extends Event<AbstractComponent> {
  public CheckedEvent(AbstractComponent component, Map<String, Object> payload) {
    super(component, payload);
  }
}
