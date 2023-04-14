package org.dwcj.component.flexlayout;

/**
 * The flex-flow property is a shorthand property for the flex-direction and flex-wrap properties.
 *
 * @see FlexDirection
 * @see FlexWrap
 * @see <a href= "https://developer.mozilla.org/en-US/docs/Web/CSS/flex-flow">flex-flow property</a>
 */
public enum FlexFlow {
  /**
   * {@link Direction#ROW} and {@link Wrap#NOWRAP}.
   */
  ROW_NOWRAP("row nowrap"),
  /**
   * {@link Direction#ROW} and {@link Wrap#WRAP}.
   */
  ROW_WRAP("row wrap"),
  /**
   * {@link Direction#ROW} and {@link Wrap#WRAP_REVERSE}.
   */
  ROW_WRAP_REVERSE("row wrap-reverse"),
  /**
   * {@link Direction#ROW_REVERSE} and {@link Wrap#NOWRAP}.
   */
  ROW_REVERSE_NOWRAP("row-reverse nowrap"),
  /**
   * {@link Direction#ROW_REVERSE} and {@link Wrap#WRAP}.
   */
  ROW_REVERSE_WRAP("row-reverse wrap"),
  /**
   * {@link Direction#ROW_REVERSE} and {@link Wrap#WRAP_REVERSE}.
   */
  ROW_REVERSE_WRAP_REVERSE("row-reverse wrap-reverse"),
  /**
   * {@link Direction#COLUMN} and {@link Wrap#NOWRAP}.
   */
  COLUMN_NOWRAP("column nowrap"),
  /**
   * {@link Direction#COLUMN} and {@link Wrap#WRAP}.
   */
  COLUMN_WRAP("column wrap"),
  /**
   * {@link Direction#COLUMN} and {@link Wrap#WRAP_REVERSE}.
   */
  COLUMN_WRAP_REVERSE("column wrap-reverse"),
  /**
   * {@link Direction#COLUMN_REVERSE} and {@link Wrap#NOWRAP}.
   */
  COLUMN_REVERSE_NOWRAP("column-reverse nowrap"),
  /**
   * {@link Direction#COLUMN_REVERSE} and {@link Wrap#WRAP}.
   */
  COLUMN_REVERSE_WRAP("column-reverse wrap"),
  /**
   * {@link Direction#COLUMN_REVERSE} and {@link Wrap#WRAP_REVERSE}.
   */
  COLUMN_REVERSE_WRAP_REVERSE("column-reverse wrap-reverse");

  private final String value;

  private FlexFlow(String value) {
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
   * Returns the {@link FlexFlow} enum value for the given string value.
   *
   * @param value the string value
   * @return the {@link FlexFlow} enum value
   */
  public static FlexFlow fromValue(String value) {
    return valueOf(value.toUpperCase().replace(" ", "_").replace("-", "_"));
  }

  /**
   * Returns the default value for this property.
   *
   * @return the default value
   */
  public static FlexFlow getDefault() {
    return ROW_NOWRAP;
  }
}
