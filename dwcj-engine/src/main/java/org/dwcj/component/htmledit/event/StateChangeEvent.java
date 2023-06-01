package org.dwcj.component.htmledit.event;

import java.util.Map;
import org.dwcj.component.AbstractComponent;
import org.dwcj.component.event.Event;

/**
 * An event that is fired when the htmlEdit changes its state.
 */
public class StateChangeEvent extends Event<AbstractComponent> {

  /**
   * Gets the name of the state that has changed as part of the event payload.
   *
   * @return the name of the state.
   */
  public String getState() {
    return (String) this.getEventMap().get("state");
  }

  /**
   * Gets the boolean value of the state that is changed as part of the event payload.
   *
   * @return the boolean value of the state that is changed.
   */
  public String getValue() {
    return (String) this.getEventMap().get("value");
  }

  /**
   * Creates a new State Change Event.
   *
   * @param component the component that fired the event
   * @param payload the event map
   */
  public StateChangeEvent(AbstractComponent component, Map<String, Object> payload) {
    super(component, payload);
  }
}
