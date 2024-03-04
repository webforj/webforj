package com.webforj.concern;

import com.webforj.component.Component;

/**
 * An interface for manipulating the position of text and images within a component relative to
 * other elements of the component. For example, when used on a CheckBox, this interface's methods
 * will control on which side of the checkbox the text appears.
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public interface HasTextPosition<T extends Component> {

  /**
   * Enum representing the text position options of a component.
   */
  enum Position {
    LEFT(8192), RIGHT(32768);

    private final int value;

    private Position(int position) {
      this.value = position;
    }

    /**
     * Gets the integer value of the text position.
     *
     * @return the integer value of text position
     */
    public int getValue() {
      return value;
    }
  }

  /**
   * Sets the position of the text within the component relative to other elements of the component.
   *
   * @param position an enum representing an internal BBj numeric constant.
   * @return the component itself.
   */
  public T setTextPosition(Position position);

  /**
   * Retrieves the value indicating the text's position relative to other elements of the component.
   *
   * @return an enum value representing text position.
   */
  public Position getTextPosition();
}
