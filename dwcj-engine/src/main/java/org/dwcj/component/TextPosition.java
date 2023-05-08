package org.dwcj.component;

/**
 * On applicable controls, creates enum which helps facilitate underlying BBj constant integers for
 * text positon behavior to legible enum values, and facilitates implementation of methods 
 * to interact with this behavior.
 */
public interface TextPosition {

  /**
   * Enum which represents the text positon of a component.
   */
  enum Position {
    LEFT(8192), RIGHT(32768);

    public final int value;

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
   * @return the control ifself
   */
  public TextPosition setTextPosition(Position position);

  /**
   * Returns a value indicating the text's position relativ to other elements of the control.
   *
   * @return Enum value of text position
   */
  public Position getTextPosition();
}
