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
  public Integer getKeyCode() {
    return (Integer) this.getEventMap().get("keyCode");
  }

  /**
   * Returns the key code with flags.
   *
   * @return the key code of the pressed key
   */
  public Integer getKeyCodeWithFlags() {
    return (Integer) this.getEventMap().get("keyCodeWithFlags");
  }

  /**
   * Returns the extended modifiers for the key that has been pressed.
   *
   * @return the extended modifiers for pressed key
   */
  public Integer getModifiersEx() {
    return (Integer) this.getEventMap().get("modifiersEx");
  }

  /**
   * Returns whether or not the alt key was pressed when the event happened.
   *
   * @return A boolean representing whether alt was pressed.
   */
  public Boolean isAltDown() {
    return (Boolean) this.getEventMap().get("altDown");
  }

  /**
   * Returns whether or not the command key was pressed when the event happened.
   *
   * @return A boolean representing whether cmd was pressed.
   */
  public Boolean isCmdDown() {
    return (Boolean) this.getEventMap().get("cmdDown");
  }

  /**
   * Returns whether or not the control key was pressed when the event happened.
   *
   * @return A boolean representing whether ctrl was pressed.
   */
  public Boolean isControlDown() {
    return (Boolean) this.getEventMap().get("controlDown");
  }

  /**
   * Returns whether or not the shift key was pressed when the event happened.
   *
   * @return A boolean representing whether shift was pressed.
   */
  public Boolean isShiftDown() {
    return (Boolean) this.getEventMap().get("shiftDown");
  }
}
