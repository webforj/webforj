package com.webforj.component.layout.flexlayout;

/**
 * The flex-wrap property specifies whether the flex items are forced in a single line or can be
 * flowed into multiple lines.
 *
 * @see <a href= "https://developer.mozilla.org/en-US/docs/Web/CSS/flex-wrap">flex-wrap property</a>
 * @author Hyyan Abo Fakher
 */
public enum FlexWrap {
  /**
   * The items will be on one line.
   */
  NOWRAP("nowrap"),
  /**
   * The items will wrap onto multiple lines, from top to bottom.
   */
  WRAP("wrap"),
  /**
   * The flex items will wrap onto multiple lines from bottom to top.
   */
  WRAP_REVERSE("wrap-reverse");

  private final String value;

  private FlexWrap(String value) {
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
   * Returns the {@link FlexWrap} enum value for the given string value.
   *
   * @param value the string value
   * @return the {@link FlexWrap} enum value
   */
  public static FlexWrap fromValue(String value) {
    return valueOf(value.toUpperCase().replace("-", "_"));
  }

  /**
   * Returns the default value for this property.
   *
   * @return the default value
   */
  public static FlexWrap getDefault() {
    return NOWRAP;
  }
}
