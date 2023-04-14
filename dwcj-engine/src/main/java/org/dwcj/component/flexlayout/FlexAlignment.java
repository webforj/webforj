package org.dwcj.component.flexlayout;

/**
 * Enum with possible values for the items alignment inside the layout.
 *
 * @see <a href= "https://developer.mozilla.org/en-US/docs/Web/CSS/align-items">align-items
 *      property</a>
 * @author Hyyan Abo Fakher
 */
public enum FlexAlignment {
  /**
   * Stretch to fill the layout (still respect min-width/max-width). This is the default value.
   */
  STRETCH("stretch"),
  /**
   * Items are placed at the start of the cross axis.
   */
  START("flex-start"),
  /**
   * Items are placed at the end of the cross axis.
   */
  END("flex-end"),
  /**
   * Items are centered in the cross-axis.
   */
  CENTER("center"),
  /**
   * Items are aligned such as their baselines align.
   */
  BASELINE("baseline"),
  /**
   * The items are evenly distributed in the container.
   */
  AUTO("auto");

  private final String value;

  private FlexAlignment(String value) {
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
   * Returns the {@link FlexAlignment} enum value for the given string value.
   *
   * @param value the string value
   * @return the {@link FlexAlignment} enum value
   */
  public static FlexAlignment fromValue(String value) {
    return valueOf(value.toUpperCase().replaceAll("^(.+?)-", ""));
  }

  /**
   * Returns the default value for this property.
   *
   * @return the default value
   */
  public static FlexAlignment getDefault() {
    return STRETCH;
  }
}
