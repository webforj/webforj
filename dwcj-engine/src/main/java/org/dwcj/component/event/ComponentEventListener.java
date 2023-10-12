package org.dwcj.component.event;

import java.io.Serializable;

/**
 * A tagging interface that all control event listeners must implement.
 *
 * @param <T> the generic type
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
@FunctionalInterface
public interface ComponentEventListener<T extends ComponentEvent<?>>
    extends java.util.EventListener, Serializable {

  /**
   * Invoked when a component event has been fired.
   *
   * @param event component event
   */
  void onComponentEvent(T event);
}
