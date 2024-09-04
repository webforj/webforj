package com.webforj.component.icons;

import java.io.Serializable;

/**
 * A factory for creating icons.
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 *
 * @see Icon
 * @see FeatherIcon
 * @see DwcIcon
 */
@FunctionalInterface
public interface IconFactory extends Serializable {
  /**
   * Create a new icon.
   *
   * @return a new icon
   */
  Icon create();

  /**
   * Get the icon pool name.
   *
   * @return the icon pool name
   */
  default String getPool() {
    return this.getClass().getSimpleName().replace("Icon", "").toLowerCase();
  }
}
