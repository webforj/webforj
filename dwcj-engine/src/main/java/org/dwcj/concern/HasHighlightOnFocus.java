package org.dwcj.concern;

import org.dwcj.component.Component;

/**
 * Interface that facilitates the implementation of text highlight behaviors for components when
 * they receive focus.
 *
 * @param <T> The type of the implementing component.
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
public interface HasHighlightOnFocus<T extends Component> {

  /**
   * Enum representing different highlight behaviors for a component.
   */
  public enum Behavior {
    /**
     * Contents of the component are never automatically highlighted when the component gets focus.
     */
    NONE(0),

    /**
     * Contents of the component are automatically highlighted when the component gets focus by
     * tabbing into it.
     */
    KEY(1),

    /**
     * Contents of the component are automatically highlighted when the component gets focus by
     * clicking into it with the mouse.
     */
    MOUSE(2),

    /**
     * Contents of the component are automatically highlighted when the component gets focus by
     * tabbing into it or clicking into it with the mouse.
     */
    KEY_MOUSE(3),

    /**
     * Contents of the component are automatically highlighted when the component gets focus under
     * program control.
     */
    FOCUS(4),

    /**
     * Contents of the component are automatically highlighted when the component gets focus under
     * program control or by tabbing into it.
     */
    FOCUS_OR_KEY(5),

    /**
     * Contents of the component are automatically highlighted when the component gets focus under
     * program control or by clicking into it with the mouse.
     */
    FOCUS_OR_MOUSE(6),

    /**
     * Contents of the component are always automatically highlighted when the component gets focus.
     */
    ALL(7);

    private final int value;

    Behavior(int value) {
      this.value = value;
    }

    /**
     * Get the code representing the highlight option.
     *
     * @return The code of the highlight option.
     */
    public int getValue() {
      return value;
    }
  }

  /**
   * Set the highlight behavior for the component's text when it receives focus.
   *
   * @param highlight The highlight behavior to set.
   * @return The component itself.
   */
  T setHighlightOnFocus(Behavior highlight);

  /**
   * Get the current highlight behavior when the component's text receives focus.
   *
   * @return The current highlight behavior.
   */
  Behavior getHighlightOnFocus();
}
