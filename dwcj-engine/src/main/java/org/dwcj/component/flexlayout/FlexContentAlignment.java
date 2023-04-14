package org.dwcj.component.flexlayout;

/**
 * Enum with the possible to set the distribution of space between and around content items along a
 * flexbox's cross-axis.
 *
 * @see <a href= "https://developer.mozilla.org/en-US/docs/Web/CSS/align-content">align-content
 *      property</a>
 * @author Hyyan Abo Fakher
 */
public enum FlexContentAlignment {
  /**
   * Items are packed in their default position as if no value was set. This is the default value.
   */
  NORMAL("normal"),
  /**
   * The items are packed toward the start of the layout.
   */
  START("flex-start"),
  /**
   * The items are packed toward the end of the layout.
   */
  END("flex-end"),
  /**
   * The items are are centered in the layout.
   */
  CENTER("center"),
  /**
   * Items evenly distributed; the first line is at the start of the container while the last one is
   * at the end.
   */
  BETWEEN("space-between"),
  /**
   * Items evenly distributed with equal space around each line.
   */
  AROUND("space-around"),
  /**
   * items are evenly distributed with equal space around them.
   */
  EVENLY("space-evenly"),
  /**
   * The lines stretch to take up the remaining space.
   */
  STRETCH("stretch");

  private final String value;

  private FlexContentAlignment(String value) {
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
   * Returns the {@link FlexContentAlignment} enum value for the given string value.
   *
   * @param value the string value
   * @return the {@link FlexContentAlignment} enum value
   */
  public static FlexContentAlignment fromValue(String value) {
    return valueOf(value.toUpperCase().replaceAll("^(.+?)-", ""));
  }

  /**
   * Returns the default value for this property.
   *
   * @return the default value
   */
  public static FlexContentAlignment getDefault() {
    return NORMAL;
  }
}
