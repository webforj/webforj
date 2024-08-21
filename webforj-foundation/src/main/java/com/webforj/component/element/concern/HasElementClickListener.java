package com.webforj.component.element.concern;

import com.webforj.component.element.ElementComposite;
import com.webforj.component.element.ElementCompositeUtil;
import com.webforj.component.element.event.ElementClickEvent;
import com.webforj.component.element.event.ElementEventOptions;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;

/**
 * A mixin interface for element components that have click listeners.
 *
 * @param <T> the component type
 *
 * @see ElementClickEvent
 * @see ElementComposite
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public interface HasElementClickListener<T extends ElementComposite> {

  /**
   * Adds a click listener to this component.
   *
   * @param listener the listener to add
   * @param options the options
   *
   * @return A listener registration for removing the event listener
   */
  public default ListenerRegistration<ElementClickEvent<T>> addClickListener(
      EventListener<ElementClickEvent<T>> listener, ElementEventOptions options) {
    return ElementCompositeUtil.addEventListener(this, ElementClickEvent.class, listener, options);
  }

  /**
   * Adds a click listener to this component.
   *
   * @param listener the listener to add
   *
   * @return A listener registration for removing the event listener
   */
  public default ListenerRegistration<ElementClickEvent<T>> addClickListener(
      EventListener<ElementClickEvent<T>> listener) {
    return addClickListener(listener, null);
  }

  /**
   * Adds an onClick listener to this component.
   *
   * @param listener the listener to add
   * @param options the options
   *
   * @return A listener registration for removing the event listener
   */
  public default ListenerRegistration<ElementClickEvent<T>> onClick(
      EventListener<ElementClickEvent<T>> listener, ElementEventOptions options) {
    return addClickListener(listener, options);
  }

  /**
   * Adds an onClick listener to this component.
   *
   * @param listener the listener to add
   *
   * @return A listener registration for removing the event listener
   */
  public default ListenerRegistration<ElementClickEvent<T>> onClick(
      EventListener<ElementClickEvent<T>> listener) {
    return addClickListener(listener);
  }
}
