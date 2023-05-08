package org.dwcj.component;


/**
 * On applicable components, creates enum which helps facilitate underlying BBj constant 
 * integers for horizontal alignment behavior to legible enum values, and facilitates 
 * implementation of methods to interact with this behavior.
 */
public interface HorizontalAlignment {

  /**
   * Enum which represents the text alignment of a component.
   */
  enum Alignment {
    LEFT(8192), MIDDLE(16384), RIGHT(32768);

    public final int value;

    private Alignment(int position) {
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
   * Returns a value indicating the text's horizontal alignment.
   *
   * @return Enum value of text alignment
   */
  Alignment getHorizontalAlignment();

  /**
   * Sets the horizontal alignment of the text within the component.
   *
   * @param alignment Enum from list representing an internal BBj numeric constant
   * @return The component itself
   */
  HorizontalAlignment setHorizontalAlignment(Alignment alignment);

}
