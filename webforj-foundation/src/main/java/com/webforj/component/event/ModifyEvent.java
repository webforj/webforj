package com.webforj.component.event;

import com.webforj.component.Component;
import java.util.Map;

/**
 * An event that is fired when a component is edited or modified.
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
