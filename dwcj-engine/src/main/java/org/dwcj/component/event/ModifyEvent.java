package org.dwcj.component.event;

import java.util.Map;
import org.dwcj.component.Component;

/**
 * An event that is fired when a component is edited.
 */
public class ModifyEvent extends ComponentEvent<Component> {

  public ModifyEvent(Component component, Map<String, Object> payload) {
    super(component, payload);
  }

  /**
   * Gets the text of the component that is sent as part of the event payload.
   *
   * @return the text of the component.
   */
  public String getText() {
    return (String) this.getEventMap().get("text");
  }
}
