package org.dwcj.component.event;

import java.util.Map;
import org.dwcj.component.AbstractComponent;

/**
 * An event that is fired when the right mouse button is pressed on a component.
 */
public class RightMouseDownEvent extends Event<AbstractComponent> {

  /**
   * Creates a new event.
   *
   * @param component the component that fired the event
   * @param payload the event map
   */
  public RightMouseDownEvent(AbstractComponent component, Map<String, Object> payload) {
    super(component, payload);
  }

  public int getMouseButton() {
    return (int) this.getEventMap().get("mouseButton");
  }

  public int getScreenX() {
    return (int) this.getEventMap().get("screenX");
  }

  public int getScreenY() {
    return (int) this.getEventMap().get("screenY");
  }

  public int getX() {
    return (int) this.getEventMap().get("x");
  }

  public int getY() {
    return (int) this.getEventMap().get("y");
  }

  public boolean isAltDown() {
    return (boolean) this.getEventMap().get("altDown");
  }

  public boolean isControlDown() {
    return (boolean) this.getEventMap().get("controlDown");
  }

  public boolean isShiftDown() {
    return (boolean) this.getEventMap().get("shiftDown");
  }
}