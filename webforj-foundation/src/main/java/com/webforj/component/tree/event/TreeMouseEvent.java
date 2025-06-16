package com.webforj.component.tree.event;

import com.webforj.component.tree.Tree;
import java.util.Map;

/**
 * An event which is fired when the user interacts with a tree node using the mouse.
 *
 * @author Hyyan Abo Fakher
 * @since 25.01
 */
public abstract class TreeMouseEvent extends TreeEvent {

  /**
   * Creates a new tree mouse event.
   *
   * @param component the component
   * @param eventMap the event map
   */
  protected TreeMouseEvent(Tree component, Map<String, Object> eventMap) {
    super(component, eventMap);
  }

  /**
   * Returns whether or not a mouse button was pressed while the event happened.
   *
   * @return An int representing the mouse Button pressed.
   */
  public int getMouseButton() {
    return (int) getEventMap().get("mouseButton");
  }

  /**
   * Returns the abscissa coordinate where this event happened relative to the parent.
   *
   * @return The x coordinate where the event happened.
   */
  public int getX() {
    return (int) getEventMap().get("x");
  }

  /**
   * Returns the ordinate coordinate where this event happened relative to the parent.
   *
   * @return The y coordinate where the event happened.
   */
  public int getY() {
    return (int) getEventMap().get("y");
  }

  /**
   * Returns whether or not the alt key was pressed when the event happened.
   *
   * @return A boolean representing whether alt was pressed.
   */
  public boolean isAltDown() {
    return (boolean) getEventMap().get("altDown");
  }

  /**
   * Returns whether or not the command key was pressed when the event happened.
   *
   * @return A boolean representing whether cmd was pressed.
   */
  public boolean isCmdDown() {
    return (boolean) getEventMap().get("cmdDown");
  }

  /**
   * Returns whether or not the control key was pressed when the event happened.
   *
   * @return A boolean representing whether ctrl was pressed.
   */
  public boolean isControlDown() {
    return (boolean) getEventMap().get("controlDown");
  }

  /**
   * Returns whether or not the shift key was pressed when the event happened.
   *
   * @return A boolean representing whether shift was pressed.
   */
  public boolean isShiftDown() {
    return (boolean) getEventMap().get("shiftDown");
  }
}
