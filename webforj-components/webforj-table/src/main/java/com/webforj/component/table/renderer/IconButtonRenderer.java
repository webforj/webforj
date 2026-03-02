package com.webforj.component.table.renderer;

import com.webforj.component.Theme;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.icons.Icon;
import com.webforj.component.icons.IconDefinition;
import com.webforj.component.icons.IconExpanse;
import com.webforj.component.table.event.renderer.RendererClickEvent;
import com.webforj.dispatcher.EventListener;

/**
 * A renderer that displays a {@code dwc-icon-button} in a table cell. The click event provides
 * access to the row item via {@code e.getItem()}.
 *
 * <pre>{@code
 * IconButtonRenderer<MusicRecord> renderer = new IconButtonRenderer<>(TablerIcon.create("edit"));
 *
 * table.addColumn("actions", r -> "").setRenderer(renderer);
 * }</pre>
 *
 * @param <T> the row data type
 *
 * @author Hyyan Abo Fakher
 * @since 25.12
 */
@NodeName("dwc-icon-button")
public class IconButtonRenderer<T> extends AbstractVoidElementRenderer<T> {
  private static final String DISABLED_ATTR = "disabled";
  private static final String EXPANSE_ATTR = "expanse";

  private String name;
  private String pool;
  private Theme theme;
  private IconExpanse expanse;
  private boolean enabled = true;

  /**
   * Creates a new icon button renderer from an {@link IconDefinition}.
   *
   * @param icon the icon definition
   * @param listener a click listener
   */
  public IconButtonRenderer(IconDefinition<?> icon, EventListener<RendererClickEvent<T>> listener) {
    setContent("");
    setName(icon.getName());
    setPool(icon.getPool());
    applyIconTheme(icon);

    if (listener != null) {
      addClickListener(listener);
    }
  }

  /**
   * Creates a new icon button renderer from an {@link IconDefinition}.
   *
   * @param icon the icon definition
   */
  public IconButtonRenderer(IconDefinition<?> icon) {
    this(icon, null);
  }

  /**
   * Sets the icon from an {@link IconDefinition}.
   *
   * @param icon the icon definition
   * @return this renderer
   */
  public IconButtonRenderer<T> setIcon(IconDefinition<?> icon) {
    setName(icon.getName());
    setPool(icon.getPool());
    applyIconTheme(icon);
    return this;
  }

  /**
   * Sets the icon name.
   *
   * @param name the icon name
   * @return this renderer
   */
  public IconButtonRenderer<T> setName(String name) {
    this.name = name;
    setAttribute("name", name);
    return this;
  }

  /**
   * Returns the icon name.
   *
   * @return the icon name
   */
  public String getName() {
    return name;
  }

  /**
   * Sets the icon pool.
   *
   * @param pool the icon pool
   * @return this renderer
   */
  public IconButtonRenderer<T> setPool(String pool) {
    this.pool = pool;
    setAttribute("pool", pool);
    return this;
  }

  /**
   * Returns the icon pool.
   *
   * @return the icon pool
   */
  public String getPool() {
    return pool;
  }

  /**
   * Sets the icon theme.
   *
   * @param theme the icon theme
   * @return this renderer
   */
  public IconButtonRenderer<T> setTheme(Theme theme) {
    this.theme = theme;
    setAttribute("theme", theme);
    return this;
  }

  /**
   * Returns the icon theme.
   *
   * @return the icon theme
   */
  public Theme getTheme() {
    return theme;
  }

  /**
   * Sets the expanse of the icon button.
   *
   * @param expanse the expanse to set
   * @return this renderer
   */
  public IconButtonRenderer<T> setExpanse(IconExpanse expanse) {
    this.expanse = expanse;
    setAttribute(EXPANSE_ATTR, expanse);
    return this;
  }

  /**
   * Returns the expanse of the icon button.
   *
   * @return the expanse
   */
  public IconExpanse getExpanse() {
    return expanse;
  }

  /**
   * Sets whether the icon button is enabled.
   *
   * @param enabled true to enable the icon button
   * @return this renderer
   */
  public IconButtonRenderer<T> setEnabled(boolean enabled) {
    this.enabled = enabled;
    if (enabled) {
      removeAttribute(DISABLED_ATTR);
    } else {
      setAttribute(DISABLED_ATTR, "true");
    }
    return this;
  }

  /**
   * Checks whether the icon button is enabled.
   *
   * @return true if enabled
   */
  public boolean isEnabled() {
    return enabled;
  }

  /** {@inheritDoc} */
  @Override
  public String build() {
    setAttribute(EXPANSE_ATTR, "", false);
    setAttribute("tab-traversable", "-1", false);
    return super.build();
  }

  private void applyIconTheme(IconDefinition<?> icon) {
    if (icon instanceof Icon iconInstance && iconInstance.getTheme() != null) {
      setTheme(iconInstance.getTheme());
    }
  }
}
