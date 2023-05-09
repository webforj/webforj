package org.dwcj.component;

/**
 * On applicable component, creates enum which helps facilitate underlying BBj constant integers for
 * text position behavior to legible enum values, and facilitates implementation of methods to
 * interact with this behavior. Text position is the location of the text in relation to the
 * component itself.
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
