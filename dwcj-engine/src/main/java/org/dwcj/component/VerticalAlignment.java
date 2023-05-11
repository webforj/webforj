package org.dwcj.component;

/**
 * Controls the vertical alignment of the content within the component, namely text and images. It
 * does not alter the position of the entire component itself.
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
