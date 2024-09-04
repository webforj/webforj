package com.webforj.component.icons;

import java.util.Objects;

/**
 * A factory for creating Tabler icons.
 *
 * @see <a href="https://tablericons.com/">Tabler Icons</a>
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public class TablerIcon {
  static final String POOL = "tabler";

  /**
   * Enumeration for the variations of the icon.
   */
  public enum Variate {
    OUTLINE, FILLED;

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
      return name().toLowerCase() + "-";
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
    Objects.requireNonNull(type, "The icon type must not be null");
    return new Icon(String.valueOf(type) + name, POOL);
  }
}
