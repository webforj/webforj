package org.dwcj.component;


/**
 * Controls the horizontal alignment of the content within the component, namely text and images. It
 * does not alter the position of the entire component itself.
 */
public interface HorizontalAlignment {

  /**
   * Enum which represents the text alignment of a component.
   */
  enum Alignment {
    LEFT(8192), MIDDLE(16384), RIGHT(32768);

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
   * Returns a value indicating the text's horizontal alignment.
   *
   * @return Enum value of text alignment
   */
  public Alignment getHorizontalAlignment();

  /**
   * Sets the horizontal alignment of the text within the component.
   *
   * @param alignment Enum from list representing an internal BBj numeric constant
   * @return The component itself
   */
  public HorizontalAlignment setHorizontalAlignment(Alignment alignment);

}
