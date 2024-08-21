package com.webforj.component.html;

import com.webforj.component.Component;
import com.webforj.component.element.Element;
import com.webforj.component.element.ElementComposite;
import com.webforj.component.element.annotation.EventName;
import com.webforj.component.element.concern.HasElementClickListener;
import com.webforj.component.element.event.ElementEventOptions;
import com.webforj.component.event.ComponentEvent;
import com.webforj.concern.HasAttribute;
import com.webforj.concern.HasClassName;
import com.webforj.concern.HasHtml;
import com.webforj.concern.HasProperty;
import com.webforj.concern.HasSize;
import com.webforj.concern.HasStyle;
import com.webforj.concern.HasText;
import com.webforj.concern.HasVisibility;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.exceptions.WebforjRuntimeException;

/**
 * Base class for a {@link Component} that represents a single built-in HTML element.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
public abstract class HtmlComponent<T extends HtmlComponent<T>> extends ElementComposite
    implements HasStyle<T>, HasAttribute<T>, HasProperty<T>, HasClassName<T>, HasText<T>,
    HasHtml<T>, HasVisibility<T>, HasSize<T>, HasElementClickListener<T> {

  /**
   * Alias for {@link #getBoundComponent()}.
   *
   * <p>
   * Gets the underlying {@link Element} instance.
   * </p>
   *
   * @return the element instance.
   */
  @Override
  public final Element getElement() {
    return super.getElement();
  }

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
   *
   * @throws WebforjRuntimeException if the event class is not annotated with {@link EventName}
   * @throws IllegalStateException if the event name is already registered with a different event
   *         class
   */
  @Override
  public <E extends ComponentEvent<?>> ListenerRegistration<E> addEventListener(
      Class<? super E> eventClass, EventListener<E> listener, ElementEventOptions options) {
    return super.addEventListener(eventClass, listener, options);
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
   *
   * @throws WebforjRuntimeException if the event class is not annotated with {@link EventName}
   */
  @Override
  public <E extends ComponentEvent<?>> ListenerRegistration<E> addEventListener(
      Class<? super E> eventClass, EventListener<E> listener) {
    return super.addEventListener(eventClass, listener);
  }
}
