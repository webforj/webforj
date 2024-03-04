package com.webforj.addons.table.renderer;

import com.webforj.addons.table.event.renderer.RendererClickEvent;
import com.webforj.dispatcher.EventListener;

/**
 * A renderer for a void element.
 *
 * @param <T> the type of the row data
 *
 * @see AbstractVoidElementRenderer
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public class VoidElementRenderer<T> extends AbstractVoidElementRenderer<T> {
  private final String nodeName;

  /**
   * Creates a new void element renderer.
   *
   * @param nodeName the node name
   * @param listener A click listener
   */
  public VoidElementRenderer(String nodeName, EventListener<RendererClickEvent<T>> listener) {
    this.nodeName = nodeName;
    addClickListener(listener);
  }

  /**
   * Creates a new tag renderer.
   *
   * @param nodeName the tag
   */
  public VoidElementRenderer(String nodeName) {
    this.nodeName = nodeName;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getNodeName() {
    return nodeName;
  }
}
