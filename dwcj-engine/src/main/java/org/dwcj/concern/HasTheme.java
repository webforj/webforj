package org.dwcj.concern;

import org.dwcj.component.Component;
import org.dwcj.component.ThemeBase;

/**
 * An interface for modifying a component's theme.
 *
 * <p>
 * This interface provides methods to set and retrieve the theme for the component.
 * </p>
 *
 * @param <T> the type of the component that implements this interface.
 * @param <V> the type of the theme.
 *
 * @see ThemeBase
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
public interface HasTheme<T extends Component, V extends Enum<V> & ThemeBase> {

  /**
   * Sets the theme of the component.
   *
   * @param theme the theme to set for the component.
   * @return the component itself.
   */
  public T setTheme(V theme);

  /**
   * Retrieves the theme of the component.
   *
   * @return the theme of the component.
   */
  public V getTheme();
}
