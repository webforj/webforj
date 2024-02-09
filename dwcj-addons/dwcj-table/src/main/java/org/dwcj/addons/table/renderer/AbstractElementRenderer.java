package org.dwcj.addons.table.renderer;

import org.dwcj.addons.table.event.renderer.RendererClickEvent;
import org.dwcj.dispatcher.EventListener;

/**
 * The base class for all renderers which render a tag with content.
 *
 * @param <T> the type of the row data
 *
 * @see AbstractVoidElementRenderer
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public abstract class AbstractElementRenderer<T> extends AbstractVoidElementRenderer<T> {

  /**
   * Creates a new tag renderer.
   *
   * @param content the content of the tag
   * @param listener the click listener
   */
  protected AbstractElementRenderer(String content, EventListener<RendererClickEvent<T>> listener) {
    setContent(content);

    if (listener != null) {
      addClickListener(listener);
    }
  }
}
