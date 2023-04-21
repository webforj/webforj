package org.dwcj.component.event;

import java.util.Map;
import org.dwcj.component.AbstractComponent;

/**
 * An event that is fired when a component is editet.
 */
public class EditModifyEvent extends Event<AbstractComponent> {

  public EditModifyEvent(AbstractComponent component, Map<String, Object> payload) {
    super(component, payload);
  }

  public String getText() {
    return (String) this.getEventMap().get("text");
  }
}
