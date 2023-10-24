package org.dwcj.component.event;

import java.util.Map;
import org.dwcj.component.Component;

/**
 * An event that is fired when a component is checked on.
 */
public class CheckEvent extends ComponentEvent<Component> {
  public CheckEvent(Component component, Map<String, Object> payload) {
    super(component, payload);
  }
}
