package com.webforj.component.icons;

import java.util.Objects;

/**
 * A factory for creating Font Awesome icons.
 *
 * @see <a href="https://fontawesome.com/">Font Awesome</a>
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public class FontAwesomeIcon {
  static final String POOL = "fa";

  /**
   * Enumeration for the variations of the icon.
   */
  public enum Variate {
    SOLID("s"), BRAND("b"), REGULAR("r");

    private final String modifier;

    Variate(String modifier) {
      this.modifier = modifier;
    }

    /**
     * Get the icon modifier.
     *
     * @return the modifier
     */
    public String getModifier() {
      return modifier;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
      return POOL + getModifier().toLowerCase() + "-";
    }
  }

  /**
   * Create a new icon with the given name.
   *
   * @param name the icon name
   * @return a new icon
   */
  public static Icon create(String name) {
    return new Icon(name, POOL);
  }

  /**
   * Create a new icon with the given name and type.
   *
   * @param name the icon name
   * @param type the icon type
   *
   * @return a new icon
   */
  public static Icon create(String name, Variate type) {
    Objects.requireNonNull(type, "The icon variate must not be null");

    return new Icon(String.valueOf(type) + name, POOL);
  }
}
