package org.dwcj.addons.table.renderer;

import org.dwcj.addons.table.event.renderer.RendererClickEvent;
import org.dwcj.component.button.ButtonTheme;
import org.dwcj.component.element.annotation.NodeName;
import org.dwcj.dispatcher.EventListener;

/**
 * Represents a renderer for a button.
 *
 * @param <T> the type of the row data
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
@NodeName("bbj-button")
public class ButtonRenderer<T> extends NativeButtonRenderer<T> {
  private ButtonTheme theme;

  /**
   * Creates a new tag button.
   *
   * @param content the content of the button
   * @param listener the click listener
   */
  public ButtonRenderer(String content, EventListener<RendererClickEvent<T>> listener) {
    super(content, listener);
  }

  /**
   * Creates a new tag button.
   *
   * @param content the content of the button
   */
  public ButtonRenderer(String content) {
    super(content);
  }

  /**
   * Creates a new tag button.
   */
  public ButtonRenderer() {
    this(null);
  }

  /**
   * Sets the theme of the button.
   *
   * @param theme the theme to set for the renderer.
   * @return the renderer itself.
   */
  public ButtonRenderer<T> setTheme(ButtonTheme theme) {
    this.theme = theme;
    setAttribute("theme", theme);
    return this;
  }

  /**
   * Retrieves the theme of the button.
   *
   * @return the theme of the button.
   */
  public ButtonTheme getTheme() {
    return theme;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String build() {
    setAttribute("expanse", "", false);
    setAttribute("tab-traversable", "-1", false);
    return super.build();
  }
}
