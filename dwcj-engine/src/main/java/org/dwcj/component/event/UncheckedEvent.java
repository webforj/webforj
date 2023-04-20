package org.dwcj.component.event;

import java.util.Map;
import org.dwcj.component.AbstractComponent;

/**
 * An event that is fired when a component is checked off.
 */
public class UncheckedEvent extends Event<AbstractComponent> {
  public UncheckedEvent(AbstractComponent component, Map<String, Object> payload) {
    super(component, payload);
  }
}
