package com.webforj.component.table.renderer;

import com.webforj.component.table.event.renderer.RendererClickEvent;
import com.webforj.dispatcher.EventListener;

/**
 * Base class for renderers that produce an HTML element with content.
 *
 * @param <T> the row data type
 *
 * @see AbstractVoidElementRenderer
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public abstract class AbstractElementRenderer<T> extends AbstractVoidElementRenderer<T> {

  /**
   * Creates a new element renderer.
   *
   * @param content the text content
   * @param listener the click listener, or {@code null}
   */
  protected AbstractElementRenderer(String content, EventListener<RendererClickEvent<T>> listener) {
    setContent(content);

    if (listener != null) {
      addClickListener(listener);
    }
  }
}
