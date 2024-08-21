package com.webforj.component.element;

import com.webforj.component.element.event.ElementEventOptions;
import com.webforj.component.event.ComponentEvent;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;

/**
 * Utility methods for {@link ElementComposite}.
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public class ElementCompositeUtil {

  private ElementCompositeUtil() {}

  /**
   * Adds an event listener to the element composite.
   *
   * @param <E> the event type
   *
   * @param elementComposite the element composite
   * @param eventClass the event class
   * @param listener the listener
   * @param options the options
   *
   * @return A listener registration for removing the event listener
   */
  public static <E extends ComponentEvent<?>> ListenerRegistration<E> addEventListener(
      Object elementComposite, Class<? super E> eventClass, EventListener<E> listener,
      ElementEventOptions options) {
    return toComposite(elementComposite).addEventListener(eventClass, listener, options);
  }

  private static ElementComposite toComposite(Object object) {
    try {
      return (ElementComposite) object;
    } catch (ClassCastException e) {
      throw new IllegalStateException("This component is not an ElementComposite");
    }
  }
}
