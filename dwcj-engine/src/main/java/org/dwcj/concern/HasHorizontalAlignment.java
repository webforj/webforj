package org.dwcj.concern;

import com.basis.bbj.proxies.sysgui.TextAlignable;
import org.dwcj.component.Component;

/**
 * An interface for controlling the horizontal alignment of content within a component, such as text
 * and images. This does not affect the position of the entire component itself.
 *
 * @param <T> The type of the component.
 *
 * @since 23.02
 * @author Hyyan Abo Fakher
 */
public interface HasHorizontalAlignment<T extends Component> {

  /**
   * Enum representing the text alignment options of a component.
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
     * @return The integer value of text alignment.
     */
    public int getValue() {
      return value;
    }

    /**
     * Gets the enum value of the text alignment.
     *
     * @param value The integer value of text alignment.
     * @return The enum value of text alignment.
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
   * Sets the horizontal alignment of text within the component.
   *
   * @param alignment An enum representing an internal BBj numeric constant.
   * @return The component itself
   */
  public T setHorizontalAlignment(Alignment alignment);

  /**
   * Returns the value indicating the text's horizontal alignment.
   *
   * @return The enum value representing text alignment.
   */
  public Alignment getHorizontalAlignment();
}
