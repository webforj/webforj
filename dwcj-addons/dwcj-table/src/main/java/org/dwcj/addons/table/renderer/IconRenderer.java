package org.dwcj.addons.table.renderer;

import org.dwcj.component.element.annotation.NodeName;

/**
 * Represents a renderer for an icon.
 *
 * @param <T> the type of the row data
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
@NodeName("bbj-icon")
public class IconRenderer<T> extends AbstractVoidElementRenderer<T> {
  private String name;
  private String pool;

  /**
   * Creates a new instance of the icon renderer.
   *
   * @param name the name of the icon
   * @param pool the pool of the icon
   */
  public IconRenderer(String name, String pool) {
    setName(name);
    setPool(pool);
  }

  /**
   * Creates a new instance of the icon renderer.
   *
   * @param name the name of the icon
   */
  public IconRenderer(String name) {
    setName(name);
  }

  /**
   * Sets the name of the icon.
   *
   * @param name the name of the icon
   * @return this renderer itself
   */
  public IconRenderer<T> setName(String name) {
    this.name = name;
    setAttribute("name", name);
    return this;
  }

  /**
   * Gets the name of the icon.
   *
   * @return the name of the icon
   */
  public String getName() {
    return name;
  }

  /**
   * Sets the pool of the icon.
   *
   * @param pool the pool of the icon
   * @return this renderer itself
   */
  public IconRenderer<T> setPool(String pool) {
    this.pool = pool;
    setAttribute("pool", pool);
    return this;
  }

  /**
   * Gets the pool of the icon.
   *
   * @return the pool of the icon
   */
  public String getPool() {
    return pool;
  }
}
