package org.dwcj.component;

/**
 * Manipulates the position of text and images within a component relative to other 
 * elements of the component. For example, when used on a CheckBox, this interface's 
 * methods will control on which side of the checkbox the text appears.
 */
public interface TextPosition {

  /**
   * Enum which represents the text position of a component.
   */
  enum Position {
    LEFT(8192), RIGHT(32768);

    private final int value;

    private Position(int position) {
      this.value = position;
    }

    /**
     * Gets the integer value of the text alignment.
     *
     * @return Integer value of text alignment
     */
    public int getValue() {
      return value;
    }
  }

  /**
   * Sets the position of the text within the control relative to other elements of the control.
   *
   * @param position Enum from list representing an internal BBj numeric constant
   * @return the control itself
   */
  public TextPosition setTextPosition(Position position);

  /**
   * Returns a value indicating the text's position relative to other elements of the control.
   *
   * @return Enum value of text position
   */
  public Position getTextPosition();
}
