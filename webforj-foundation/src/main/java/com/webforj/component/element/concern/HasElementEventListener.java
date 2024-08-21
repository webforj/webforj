package com.webforj.component.element.concern;

import com.webforj.component.element.ElementCompositeUtil;
import com.webforj.component.element.annotation.EventName;
import com.webforj.component.element.event.ElementEventOptions;
import com.webforj.component.event.ComponentEvent;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;

/**
 * A mixin interface for element components that have custom event listeners.
 *
 * @see EventName
 * @see ElementEventOptions
 * @see EventListener
 * @see ElementComposite
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public interface HasElementEventListener {

  /**
   * Adds a listener for the given event type.
   *
   * @param <E> The event type
   *
   * @param eventClass The event class
   * @param listener The listener
   * @param options The event options
   *
   * @return A listener registration for removing the event listener
   */
  public default <E extends ComponentEvent<?>> ListenerRegistration<E> addEventListener(
      Class<? super E> eventClass, EventListener<E> listener, ElementEventOptions options) {
    return ElementCompositeUtil.addEventListener(this, eventClass, listener, options);
  }

  /**
   * Adds a listener for the given event type.
   *
   * @param <E> The event type
   *
   * @param eventClass The event class
   * @param listener The listener
   *
   * @return A listener registration for removing the event listener
   */
  public default <E extends ComponentEvent<?>> ListenerRegistration<E> addEventListener(
      Class<? super E> eventClass, EventListener<E> listener) {
    return addEventListener(eventClass, listener, null);
  }
}
