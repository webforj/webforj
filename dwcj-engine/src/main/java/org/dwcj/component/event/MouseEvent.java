package org.dwcj.component.event;

import java.util.Map;
import org.dwcj.component.Component;


/**
 * An event that is fired when the mouse enters or exits a component.
 */
public class MouseEvent extends ComponentEvent<Component> {

  /**
   * Creates a new event.
   *
   * @param component the component that fired the event
   * @param payload the event map
   */
  public MouseEvent(Component component, Map<String, Object> payload) {
    super(component, payload);
  }

  /**
   * Returns whether or not a mouse button was pressed while the event happened.
   *
   * @return An int representing the mouse Button pressed.
   */
  public int getMouseButton() {
    return (int) this.getEventMap().get("mouseButton");
  }

  /**
   * Returns the absolute abscissa screen coordinate where this event happened.
   *
   * @return The x coordinate where the event happened.
   */
  public int getScreenX() {
    return (int) this.getEventMap().get("screenX");
  }

  /**
   * Returns the absolute ordinate screen coordinate where this event happened.
   *
   * @return The y coordinate where the event happened.
   */
  public int getScreenY() {
    return (int) this.getEventMap().get("screenY");
  }

  /**
   * Returns the abscissa coordinate where this event happened relative to the parent.
   *
   * @return The x coordinate where the event happened.
   */
  public int getX() {
    return (int) this.getEventMap().get("x");
  }

  /**
   * Returns the ordinate coordinate where this event happened relative to the parent.
   *
   * @return The y coordinate where the event happened.
   */
  public int getY() {
    return (int) this.getEventMap().get("y");
  }

  /**
   * Returns whether or not the alt key was pressed when the event happened.
   *
   * @return A boolean representing whether alt was pressed.
   */
  public boolean isAltDown() {
    return (boolean) this.getEventMap().get("altDown");
  }

  /**
   * Returns whether or not the command key was pressed when the event happened.
   *
   * @return A boolean representing whether cmd was pressed.
   */
  public boolean isCmdDown() {
    return (boolean) this.getEventMap().get("cmdDown");
  }

  /**
   * Returns whether or not the control key was pressed when the event happened.
   *
   * @return A boolean representing whether ctrl was pressed.
   */
  public boolean isControlDown() {
    return (boolean) this.getEventMap().get("controlDown");
  }

  /**
   * Returns whether or not the shift key was pressed when the event happened.
   *
   * @return A boolean representing whether shift was pressed.
   */
  public boolean isShiftDown() {
    return (boolean) this.getEventMap().get("shiftDown");
  }
}
