package org.dwcj.component.flexlayout;

/**
 * Possible values for the flex-direction property.
 *
 * @see <a href= "https://developer.mozilla.org/en-US/docs/Web/CSS/flex-direction">flex-direction
 *      property</a>
 * @author Hyyan Abo Fakher
 */
public enum FlexDirection {
  /**
   * Left to right in ltr; right to left in rtl. This is the default value.
   */
  ROW("row"),
  /**
   * Right to left in ltr; left to right in rtl
   */
  ROW_REVERSE("row-reverse"),
  /**
   * Same as {@link #ROW} but top to bottom
   */
  COLUMN("column"),
  /**
   * Same as {@link #ROW_REVERSE} but bottom to top
   */
  COLUMN_REVERSE("column-reverse");

  private final String value;

  private FlexDirection(String value) {
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
   * Returns the {@link FlexDirection} enum value for the given string value.
   *
   * @param value the string value
   * @return the {@link FlexDirection} enum value
   */
  public static FlexDirection fromValue(String value) {
    return valueOf(value.toUpperCase());
  }

  /**
   * Returns the default value for direction.
   *
   * @return the default value
   */
  public static FlexDirection getDefault() {
    return ROW;
  }
}
