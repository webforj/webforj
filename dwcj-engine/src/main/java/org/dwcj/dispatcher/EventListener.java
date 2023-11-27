package org.dwcj.dispatcher;

import java.io.Serializable;
import java.util.EventObject;

/**
 * A tagging interface that all event listeners must implement.
 *
 * @param <T> the generic type
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
@FunctionalInterface
public interface EventListener<T extends EventObject>
    extends java.util.EventListener, Serializable {

  /**
   * Invoked when a component event has been fired.
   *
   * @param event component event
   */
  void onEvent(T event);
}
