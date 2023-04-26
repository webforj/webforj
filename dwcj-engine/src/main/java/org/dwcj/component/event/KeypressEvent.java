package org.dwcj.component.event;

import java.util.Map;
import org.dwcj.component.AbstractComponent;

/**
 * An event that is fired when a key is pressed.
 */
public class KeypressEvent extends Event<AbstractComponent> {

  public KeypressEvent(AbstractComponent component, Map<String, Object> payload) {
    super(component, payload);
  }

  /**
   * Returns the key code of the key which has been pressed.
   *
   * @return the code of the pressed key
   */
  public int getKeyCode() {
    return (int) this.getEventMap().get("keyCode");
  }

  /**
   * Returns whether or not the alt key was pressed when the event happened.
   *
   * @return A boolean representing whether alt was pressed.
   */
  public boolean isAltKey() {
    return (boolean) this.getEventMap().get("altKey");
  }

  /**
   * Returns whether or not the command key was pressed when the event happened.
   *
   * @return A boolean representing whether cmd was pressed.
   */
  public boolean isCmdKey() {
    return (boolean) this.getEventMap().get("cmdKey");
  }

  /**
   * Returns whether or not the control key was pressed when the event happened.
   *
   * @return A boolean representing whether ctrl was pressed.
   */
  public boolean isControlKey() {
    return (boolean) this.getEventMap().get("controlKey");
  }

  /**
   * Returns whether or not the shift key was pressed when the event happened.
   *
   * @return A boolean representing whether shift was pressed.
   */
  public boolean isShiftKey() {
    return (boolean) this.getEventMap().get("shiftKey");
  }
}
