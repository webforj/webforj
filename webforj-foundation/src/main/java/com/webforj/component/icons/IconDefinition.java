package com.webforj.component.icons;

/**
 * An interface for defining an icon component in webforJ.
 *
 * @author Hyyan Abo Fakher
 * @since 25.01
 */
public interface IconDefinition<T> {
  /**
   * Sets the icon name.
   *
   * @param name the icon name
   * @return the component itself
   */
  public T setName(String name);

  /**
   * Gets the icon name.
   *
   * @return the icon name
   */
  public String getName();

  /**
   * Sets the icon pool.
   *
   * @param pool the icon pool
   * @return the component itself
   */
  public T setPool(String pool);

  /**
   * Gets the icon pool.
   *
   * @return the icon pool
   */
  public String getPool();
}

