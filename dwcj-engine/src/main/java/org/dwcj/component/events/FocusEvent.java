package org.dwcj.component.events;

import java.util.Map;
import org.dwcj.component.AbstractDwcComponent;

/**
 * A focus event for the controls that can be focused.
 */
public class FocusEvent extends Event<AbstractDwcComponent> {

  /**
   * Creates a new event.
   *
   * @param control the control
   * @param payload the event map
   */
  public FocusEvent(AbstractDwcComponent control, Map<String, Object> payload) {
    super(control, payload);
  }

  /**
   * Gets the text of the component that is sent as part of the event payload.
   *
   * @return the text of the component.
   */
  public String getText() {
    return (String) this.getEventMap().get("text");
  }

  /**
   * Getter for the state of the focus. 
   *
   * @return a boolean indicating wether the focus changed is permanent or temporary
   */
  /*
  public Boolean isTemporary() {
    return (Boolean) this.getEventMap().get("temporary");
  }
  */
}
