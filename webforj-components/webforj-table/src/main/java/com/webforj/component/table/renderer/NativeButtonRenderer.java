package com.webforj.component.table.renderer;

import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.table.event.renderer.RendererClickEvent;
import com.webforj.dispatcher.EventListener;

/**
 * Represents a renderer for a native html button.
 *
 * @param <T> the type of the row data
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
@NodeName("button")
public class NativeButtonRenderer<T> extends AbstractElementRenderer<T> {

  /**
   * Creates a new tag button.
   *
   * @param content the content of the button
   * @param listener the click listener
   */
  public NativeButtonRenderer(String content, EventListener<RendererClickEvent<T>> listener) {
    super(content, listener);
  }

  /**
   * Creates a new tag button.
   *
   * @param content the content of the button
   */
  public NativeButtonRenderer(String content) {
    this(content, null);
  }

  /**
   * Creates a new tag button.
   */
  public NativeButtonRenderer() {
    this(null);
  }

  /**
   * Enables or disables the renderer.
   *
   * @param enabled true to enable, false to disable
   *
   * @return the renderer
   */
  public AbstractVoidElementRenderer<T> setEnabled(boolean enabled) {
    String key = "disabled";

    if (enabled) {
      removeAttribute(key);
    } else {
      setAttribute(key, "true");
    }

    return this;
  }

  /**
   * Checks if the renderer is enabled.
   *
   * @return true if the renderer is enabled, false otherwise
   */
  public boolean isEnabled() {
    return getAttribute("disabled") != null;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String build() {
    setAttribute("tab-traversable", "-1", false);
    return super.build();
  }
}
