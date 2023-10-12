package org.dwcj.concern.legacy;

/**
 * On applicable controls, creates enum which helps facilitate underlying BBj constant integers for
 * scroll wheel behavior to legible enum values, and facilitates implementation of methods to
 * interact with this behavior.
 *
 * @deprecated Dwc components don't support mouse wheel behavior.
 */
@Deprecated(since = "23.05", forRemoval = true)
public interface LegacyHasMouseWheelCondition {

  /**
   * Enum which maps to BBj constants for scroll wheel behavior.
   */
  enum MouseWheelCondition {
    DEFAULT(0), NEVER(1), FOCUS(2), MOUSE_OVER(3), FOCUS_AND_MOUSE_OVER(4), MOUSE_THEN_FOCUS(
        5), FOCUS_THEN_MOUSE(6);

    public final Integer mouseWheelEnabledCondition;

    private MouseWheelCondition(Integer condition) {
      this.mouseWheelEnabledCondition = condition;
    }
  }

  /**
   * getScrollWheelBehavior returns a constant indicating under what situations the control will
   * respond to the mouse scroll wheel.
   *
   * @return Enum value of scroll behavior
   */
  MouseWheelCondition getScrollWheelBehavior();

  /**
   * setScrollWheelBehavior configures under what situations the control will respond to the mouse
   * scroll wheel.
   *
   * @param condition Enum value for desired behavior
   * @return The control itself
   */
  LegacyHasMouseWheelCondition setScrollWheelBehavior(MouseWheelCondition condition);
}
