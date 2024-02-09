package org.dwcj.addons.table.event.renderer;

import java.util.EventObject;

/**
 * Represents a change event for a renderer.
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public class RendererChangeEvent extends EventObject {

  /**
   * Constructs a new {@code RendererChangeEvent}.
   *
   * @param source the source of the event
   */
  public RendererChangeEvent(Object source) {
    super(source);
  }
}
