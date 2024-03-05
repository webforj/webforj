package com.webforj.component.navigator.event;

import com.webforj.component.navigator.Navigator;
import java.util.Map;

/**
 * An event fired when the navigator direction is changed.
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public class NavigatorChangeEvent extends NavigatorMoveEvent {

  /**
   * An enum describing the moving direction of the navigator.
   */
  public enum Direction {
    /** The navigator direction is changed to first page. */
    FIRST,
    /** The navigator direction is changed to last page. */
    LAST,
    /** The navigator direction is changed to next page. */
    NEXT,
    /** The navigator direction is changed to previous page. */
    PREVIOUS
  }

  private final Direction direction;

  /**
   * Creates a new event.
   *
   * @param component the component
   * @param direction the direction
   * @param current the current index
   * @param startIndex the start index
   * @param endIndex the end index
   */
  public NavigatorChangeEvent(Navigator component, Direction direction, int current, int startIndex,
      int endIndex) {
    super(component, Map.of("current", current, "startIndex", startIndex, "endIndex", endIndex));
    this.direction = direction;
  }

  /**
   * Returns the navigation direction.
   *
   * @return the direction
   */
  public Direction getDirection() {
    return direction;
  }
}
