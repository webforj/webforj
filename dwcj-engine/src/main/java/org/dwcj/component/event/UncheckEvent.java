package org.dwcj.component.event;

import java.util.Map;
import org.dwcj.component.Component;

/**
 * An event that is fired when a component is checked off.
 */
public class UncheckEvent extends ComponentEvent<Component> {
  public UncheckEvent(Component component, Map<String, Object> payload) {
    super(component, payload);
  }
}
