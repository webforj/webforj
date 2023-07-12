package org.dwcj.component;

/**
 * Interface facilitates implementation of behaviors to modify a component's theme.
 *
 * @param <T> the type of the component
 * @param <V> the type of the theme
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
public interface HasTheme<T extends Component, V extends Enum<V> & ThemeBase> {

  /**
   * Set the theme of the component.
   *
   * @param theme the theme to set for the component
   * @return the component itself
   */
  public T setTheme(V theme);

  /**
   * Returns the theme of the component.
   *
   * @return the theme of the component
   */
  public V getTheme();
}
