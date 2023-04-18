package org.dwcj.component.event;

import java.io.Serializable;

/**
 * A tagging interface that all control event listeners must implement.
 *
 * @param <T> the generic type
 *
 * @author Hyyan Abo Fakher
 */
@FunctionalInterface
public interface EventListener<T extends Event<?>> extends java.util.EventListener, Serializable {

  /**
   * Invoked when a control event has been fired.
   *
   * @param event component event
   */
  void execute(T event);
}
