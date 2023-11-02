package org.dwcj.component.event;

import java.util.Map;
import org.dwcj.component.Component;

/**
 * An event that is triggered when an element loses focus, opposite of a Focus event. It occurs 
 * when the user interacts with an element, such as clicking inside an input field, and then 
 * moves the focus away from that  * element, typically by clicking outside of it or tabbing 
 * to another element on the page. This event is useful when you want to detect when a user 
 * moves away from a particular element on a web page, such as an input field or a button. 
 * 
 * @see FocusEvent
 */
public class BlurEvent extends ComponentEvent<Component> {

  /**
   * Creates a new Blur event.
   *
   * @param component the component that fired the event
   * @param payload the event map
   */
  public BlurEvent(Component component, Map<String, Object> payload) {
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
