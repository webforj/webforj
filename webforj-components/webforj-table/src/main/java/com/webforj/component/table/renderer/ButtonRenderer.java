package com.webforj.component.table.renderer;

import com.webforj.component.button.ButtonTheme;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.table.event.renderer.RendererClickEvent;
import com.webforj.dispatcher.EventListener;

/**
 * A renderer that displays a {@code dwc-button} in a table cell.
 *
 * <pre>{@code
 * ButtonRenderer<MusicRecord> renderer = new ButtonRenderer<>("Edit");
 * renderer.setTheme(ButtonTheme.PRIMARY);
 *
 * table.addColumn("edit", r -> "Edit").setRenderer(renderer);
 * }</pre>
 *
 * @param <T> the row data type
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
@NodeName("dwc-button")
public class ButtonRenderer<T> extends NativeButtonRenderer<T> {
  private ButtonTheme theme;

  /**
   * Creates a new button renderer with the given content and click listener.
   *
   * @param content the content of the button
   * @param listener the click listener
   */
  public ButtonRenderer(String content, EventListener<RendererClickEvent<T>> listener) {
    super(content, listener);
  }

  /**
   * Creates a new button renderer with the given content.
   *
   * @param content the content of the button
   */
  public ButtonRenderer(String content) {
    super(content);
  }

  /**
   * Creates a new button renderer.
   */
  public ButtonRenderer() {
    this(null);
  }

  /**
   * Sets the theme of the button.
   *
   * @param theme the theme to set
   * @return this renderer
   */
  public ButtonRenderer<T> setTheme(ButtonTheme theme) {
    this.theme = theme;
    setAttribute("theme", theme);
    return this;
  }

  /**
   * Returns the theme of the button.
   *
   * @return the theme
   */
  public ButtonTheme getTheme() {
    return theme;
  }

  /** {@inheritDoc} */
  @Override
  public String build() {
    setAttribute("expanse", "", false);
    setAttribute("tab-traversable", "-1", false);
    return super.build();
  }
}
