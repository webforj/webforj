package org.dwcj.component;

import org.dwcj.concern.HasTheme;

/**
 * An interface for components that support the theme attribute.
 *
 * @see HasTheme
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
public interface ThemeBase {

  /**
   * Retrieves the current theme.
   *
   * @return the current theme of the component.
   */
  public String getValue();
}
