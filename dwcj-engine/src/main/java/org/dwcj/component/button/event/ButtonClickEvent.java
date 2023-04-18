package org.dwcj.component.button.event;

import java.util.Map;
import org.dwcj.component.button.Button;
import org.dwcj.component.event.Event;

/**
 * A push, or click, event for the Button class.
 */
public class ButtonClickEvent extends Event<Button> {

  /**
   * Creates a new event.
   *
   * @param component the component
   * @param payload the event map
   */
  public ButtonClickEvent(Button component, Map<String, Object> payload) {
    super(component, payload);
  }

  /**
   * Getter for the x coordinate.
   *
   * @return the x coordinate
   */
  public double getX() {
    return (double) this.getEventMap().get("x");
  }

  /**
   * Getter for the y coordinate.
   *
   * @return the y coordinate
   */
  public double getY() {
    return (double) this.getEventMap().get("y");
  }
}

