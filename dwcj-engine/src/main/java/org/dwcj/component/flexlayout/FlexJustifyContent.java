package org.dwcj.component.flexlayout;

/**
 * Enum with possible values for the way the space inside the layout is distributed between and
 * around the controls along the main axis of the layout.
 *
 * @see <a href= "https://developer.mozilla.org/en-US/docs/Web/CSS/justify-content">justify-content
 *      property</a>
 * @author Hyyan Abo Fakher
 */
public enum FlexJustifyContent {
  /**
   * Items are packed toward the start of the layout direction. This is the default value.
   */
  START("flex-start"),
  /**
   * Items are packed toward the end of the layout direction.
   */
  END("flex-end"),
  /**
   * Items are centered along the line.
   */
  CENTER("center"),
  /**
   * Items are evenly distributed in the line; first item is on the start line, last item on the end
   * line.
   */
  BETWEEN("space-between"),
  /**
   * Items are evenly distributed in the line with equal space around them.
   *
   * <p>
   * Note that visually the spaces aren’t equal, since all the items have equal space on both sides.
   * The first item will have one unit of space against the container edge, but two units of space
   * between the next item because that next item has its own spacing that applies.
   * </p>
   */
  AROUND("space-around"),

  /**
   * Items are distributed so that the spacing between any two items (and the space to the edges) is
   * equal.
   */
  EVENLY("space-evenly");

  private final String value;

  private FlexJustifyContent(String value) {
    this.value = value;
  }

  /**
   * Get the value of this property.
   *
   * @return the value
   */
  public String getValue() {
    return value;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String toString() {
    return value;
  }

  /**
   * Returns the {@link FlexJustifyContent} enum value for the given string value.
   *
   * @param value the string value
   * @return the {@link FlexJustifyContent} enum value
   */
  public static FlexJustifyContent fromValue(String value) {
    return valueOf(value.toUpperCase().replaceAll("^(.+?)-", ""));
  }

  /**
   * Returns the default value for this property.
   *
   * @return the default value
   */
  public static FlexJustifyContent getDefault() {
    return START;
  }
}
