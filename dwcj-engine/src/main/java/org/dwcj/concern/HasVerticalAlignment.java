package org.dwcj.concern;

import org.dwcj.component.Component;

/**
 * An interface for controlling the vertical alignment of text and images within a component. It
 * does not alter the position of the entire component itself.
 *
 * <p>
 * This interface provides methods to set and retrieve the vertical alignment of text and images
 * within the component.
 * </p>
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public interface HasVerticalAlignment<T extends Component> {

  /**
   * Enum representing the vertical alignment options of a component.
   */
  enum Alignment {
    TOP(8192), CENTER(16384), BOTTOM(32768);

    private final int value;

    private Alignment(int position) {
      this.value = position;
    }

    /**
     * Gets the integer value of the vertical alignment.
     *
     * @return the integer value of vertical alignment.
     */
    public int getValue() {
      return value;
    }
  }

  /**
   * Sets the vertical alignment of the text within the component.
   *
   * @param alignment an enum representing an internal BBj numeric constant.
   * @return the component itself after configuring the vertical alignment.
   */
  public T setVerticalAlignment(Alignment alignment);

  /**
   * Retrieves the value indicating the text's vertical alignment.
   *
   * @return an enum value representing vertical alignment.
   */
  public Alignment getVerticalAlignment();
}
