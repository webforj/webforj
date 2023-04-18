package org.dwcj.component.events;

import java.util.Map;
import org.dwcj.component.AbstractDwcComponent;

/**
 * An event that is fired when a component loses focus.
 */
public class BlurEvent extends Event<AbstractDwcComponent> {

  /**
   * Creates a new Blur event.
   *
   * @param component the component that fired the event
   * @param payload the event map
   */
  public BlurEvent(AbstractDwcComponent component, Map<String, Object> payload) {
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

  /**
   * Returns the result of the client validation function, if any, when the control loses focus.
   *
   * @return the result of the client validation, if any
   */
  public Boolean isClientValidationValid() {
    return (Boolean) this.getEventMap().get("client-validation-valid");
  }
}
