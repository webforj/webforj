package org.dwcj.component.event;

import java.util.Map;
import org.dwcj.component.AbstractComponent;

/**
 * An event that is fired when a component is checked off.
 */
public class CheckedChangedEvent extends Event<AbstractComponent> {
  public CheckedChangedEvent(AbstractComponent component, Map<String, Object> payload) {
    super(component, payload);
  }
}
