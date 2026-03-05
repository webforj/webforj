package com.webforj.component.table.renderer;

import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.icons.Icon;
import com.webforj.component.icons.IconDefinition;
import com.webforj.component.table.event.renderer.RendererClickEvent;
import com.webforj.dispatcher.EventListener;

/**
 * A renderer that displays a {@code dwc-icon} in a table cell.
 *
 * <pre>{@code
 * IconRenderer<MusicRecord> renderer = new IconRenderer<>(TablerIcon.create("music"));
 *
 * table.addColumn("type", MusicRecord::getMusicType).setRenderer(renderer);
 * }</pre>
 *
 * @param <T> the row data type
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
@NodeName("dwc-icon")
public class IconRenderer<T> extends AbstractVoidElementRenderer<T> {
  private String name;
  private String pool;

  /**
   * Creates a new instance of the icon renderer from an {@link IconDefinition}.
   *
   * @param icon the icon definition
   * @param listener A click listener for the icon
   *
   * @since 25.12
   */
  public IconRenderer(IconDefinition<?> icon, EventListener<RendererClickEvent<T>> listener) {
    setName(icon.getName());
    setPool(icon.getPool());
    applyIconTheme(icon);

    if (listener != null) {
      addClickListener(listener);
    }
  }

  /**
   * Creates a new instance of the icon renderer from an {@link IconDefinition}.
   *
   * @param icon the icon definition
   *
   * @since 25.12
   */
  public IconRenderer(IconDefinition<?> icon) {
    this(icon, null);
  }

  /**
   * Creates a new instance of the icon renderer.
   *
   * @param name the name of the icon
   * @param pool the pool of the icon
   * @param listener A click listener for the icon
   *
   * @deprecated Use {@link #IconRenderer(IconDefinition, EventListener)} instead.
   */
  @Deprecated(since = "25.12", forRemoval = true)
  public IconRenderer(String name, String pool, EventListener<RendererClickEvent<T>> listener) {
    this(new Icon(name, pool), listener);
  }

  /**
   * Creates a new instance of the icon renderer.
   *
   * @param name the name of the icon
   * @param pool the pool of the icon
   *
   * @deprecated Use {@link #IconRenderer(IconDefinition)} instead.
   */
  @Deprecated(since = "25.12", forRemoval = true)
  public IconRenderer(String name, String pool) {
    this(new Icon(name, pool));
  }

  /**
   * Creates a new instance of the icon renderer.
   *
   * @param name the name of the icon
   * @param listener A click listener for the icon
   *
   * @deprecated Use {@link #IconRenderer(IconDefinition, EventListener)} instead.
   */
  @Deprecated(since = "25.12", forRemoval = true)
  public IconRenderer(String name, EventListener<RendererClickEvent<T>> listener) {
    this(new Icon(name, "tabler"), listener);
  }

  /**
   * Creates a new instance of the icon renderer.
   *
   * @param name the name of the icon
   *
   * @deprecated Use {@link #IconRenderer(IconDefinition)} instead.
   */
  @Deprecated(since = "25.12", forRemoval = true)
  public IconRenderer(String name) {
    this(new Icon(name, "tabler"));
  }

  /**
   * Sets the icon from an {@link IconDefinition}.
   *
   * @param icon the icon definition
   * @return this renderer
   *
   * @since 25.12
   */
  public IconRenderer<T> setIcon(IconDefinition<?> icon) {
    setName(icon.getName());
    setPool(icon.getPool());
    applyIconTheme(icon);
    return this;
  }

  /**
   * Sets the name of the icon.
   *
   * @param name the name of the icon
   * @return this renderer
   */
  public IconRenderer<T> setName(String name) {
    this.name = name;
    setAttribute("name", name);
    return this;
  }

  /**
   * Returns the name of the icon.
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
   * @return this renderer
   */
  public IconRenderer<T> setPool(String pool) {
    this.pool = pool;
    setAttribute("pool", pool);
    return this;
  }

  /**
   * Returns the pool of the icon.
   *
   * @return the pool of the icon
   */
  public String getPool() {
    return pool;
  }

  private void applyIconTheme(IconDefinition<?> icon) {
    if (icon instanceof Icon iconInstance && iconInstance.getTheme() != null) {
      setAttribute("theme", iconInstance.getTheme());
    }
  }
}
