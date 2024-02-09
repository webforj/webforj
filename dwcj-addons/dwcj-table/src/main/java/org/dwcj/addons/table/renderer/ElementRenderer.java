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
public class ElementRenderer<T> extends AbstractElementRenderer<T> {
  private final String nodeName;

  /**
   * Creates a new element renderer.
   *
   * @param nodeName the node name
   * @param content the content of the node
   * @param listener A click listener
   */
  public ElementRenderer(String nodeName, String content,
      EventListener<RendererClickEvent<T>> listener) {
    super(content, listener);
    this.nodeName = nodeName;
  }

  /**
   * Creates a new element renderer.
   *
   * @param nodeName the node name
   * @param content the content of the node
   */
  public ElementRenderer(String nodeName, String content) {
    super(content, null);
    this.nodeName = nodeName;
  }

  /**
   * Creates a new element renderer.
   *
   * @param nodeName the node name
   */
  public ElementRenderer(String nodeName) {
    super(null, null);
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
