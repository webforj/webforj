package org.dwcj.component;

import com.basis.bbj.proxies.sysgui.TextAlignable;

/**
 * Controls the horizontal alignment of the content within the component, namely text and images. It
 * does not alter the position of the entire component itself.
 *
 * @param <T> The component type
 *
 * @since 23.02
 * @author Hyyan Abo Fakher
 */
public interface HorizontalAlignment<T extends Component> {

  /**
   * Enum which represents the text alignment of a component.
   */
  enum Alignment {
    LEFT(TextAlignable.ALIGN_LEFT), MIDDLE(TextAlignable.ALIGN_CENTER), RIGHT(
        TextAlignable.ALIGN_RIGHT);

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

    /**
     * Gets the enum value of the text alignment.
     *
     * @param value Integer value of text alignment
     * @return Enum value of text alignment
     */
    public static Alignment fromValue(int value) {
      for (Alignment alignment : values()) {
        if (alignment.getValue() == value) {
          return alignment;
        }
      }
      return null;
    }
  }

  /**
   * Sets the horizontal alignment of the text within the component.
   *
   * @param alignment Enum from list representing an internal BBj numeric constant
   * @return The component itself
   */
  public T setHorizontalAlignment(Alignment alignment);

  /**
   * Returns a value indicating the text's horizontal alignment.
   *
   * @return Enum value of text alignment
   */
  public Alignment getHorizontalAlignment();
}
