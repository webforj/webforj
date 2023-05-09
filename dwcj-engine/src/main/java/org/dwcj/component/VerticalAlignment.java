package org.dwcj.component;

/**
 * On applicable components, creates enum which helps facilitate underlying BBj constant integers
 * for vertical alignment behavior to legible enum values, and facilitates implementation of methods
 * to interact with this behavior.
 */
public interface VerticalAlignment {

  /**
   * Enum which represents the text alignment of a component.
   */
  enum Alignment {
    TOP(8192), CENTER(16384), BOTTOM(32768);

    private final int value;

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
   * Sets the vertical alignment of the text within the component.
   *
   * @param alignment Enum from list representing an internal BBj numeric constant
   * @return the control itself
   */
  public VerticalAlignment setVerticalAlignment(Alignment alignment);

  /**
   * Returns a value indication the text's vertical alignment.
   *
   * @return Enum value of text alignment
   */
  public Alignment getVerticalAlignment();
}
