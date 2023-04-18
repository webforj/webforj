package org.dwcj.component.event;

import java.util.Map;
import org.dwcj.component.AbstractDwcComponent;

/**
 * An event that is fired when a component gains focus.
 */
public class FocusEvent extends Event<AbstractDwcComponent> {

  /**
   * Creates a new event.
   *
   * @param component the component that fired the event
   * @param payload the event map
   */
  public FocusEvent(AbstractDwcComponent component, Map<String, Object> payload) {
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
