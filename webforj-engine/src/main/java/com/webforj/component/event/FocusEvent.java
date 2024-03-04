package com.webforj.component.event;

import com.webforj.component.Component;
import java.util.Map;

/**
 * An event that is fired when an element gains focus, opposite of a blur event.
 *
 * <p>
 * This event occurs when the user interacts with an element, typically by clicking inside an input
 * field or navigating to it using the keyboard's tab key, causing the element to become active and
 * ready to receive user input.
 * </p>
 *
 * @see BlurEvent
 */
public class FocusEvent extends ComponentEvent<Component> {

  /**
   * Creates a new event.
   *
   * @param component the component that fired the event
   * @param payload the event map
   */
  public FocusEvent(Component component, Map<String, Object> payload) {
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
