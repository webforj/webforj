package org.dwcj.component.events;

import java.util.Map;
import org.dwcj.component.AbstractDwcComponent;

/**
 * A blur event for applicable Components.
 */
public class BlurEvent extends Event<AbstractDwcComponent> {
  
  /**
   * Creates a new Blur event.
   *
   * @param control the control
   * @param payload the event map
   */
  public BlurEvent(AbstractDwcComponent control, Map<String, Object> payload) {
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
   * Returns the result of the client validation function, if any, when the control loses focus.
   *
   * @return the result of the client validation, if any
   */
  public Boolean isClientValidationValid() {
    return (Boolean) this.getEventMap().get("client-validation-valid");
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
