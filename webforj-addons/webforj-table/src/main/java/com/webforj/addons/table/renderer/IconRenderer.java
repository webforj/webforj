package com.webforj.addons.table.renderer;

import com.webforj.addons.table.event.renderer.RendererClickEvent;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.dispatcher.EventListener;

/**
 * Represents a renderer for an icon.
 *
 * @param <T> the type of the row data
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
@NodeName("dwc-icon")
public class IconRenderer<T> extends AbstractVoidElementRenderer<T> {
  private String name;
  private String pool;

  /**
   * Creates a new instance of the icon renderer.
   *
   * @param name the name of the icon
   * @param pool the pool of the icon
   * @param listener A click listener for the icon
   */
  public IconRenderer(String name, String pool, EventListener<RendererClickEvent<T>> listener) {
    setName(name);
    setPool(pool);

    if (listener != null) {
      addClickListener(listener);
    }
  }

  /**
   * Creates a new instance of the icon renderer.
   *
   * @param name the name of the icon
   * @param pool the pool of the icon
   */
  public IconRenderer(String name, String pool) {
    this(name, pool, null);
  }

  /**
   * Creates a new instance of the icon renderer.
   *
   * @param name the name of the icon
   * @param listener A click listener for the icon
   */
  public IconRenderer(String name, EventListener<RendererClickEvent<T>> listener) {
    this(name, "tabler", listener);
  }

  /**
   * Creates a new instance of the icon renderer.
   *
   * @param name the name of the icon
   */
  public IconRenderer(String name) {
    this(name, "tabler", null);
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
