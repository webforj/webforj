package org.dwcj.component.html;

import java.lang.reflect.Type;
import org.dwcj.component.Component;
import org.dwcj.component.element.Element;
import org.dwcj.component.element.ElementComposite;
import org.dwcj.component.element.annotation.EventName;
import org.dwcj.component.element.event.ElementEventOptions;
import org.dwcj.component.event.ComponentEvent;
import org.dwcj.component.html.event.HtmlClickEvent;
import org.dwcj.concern.HasAttribute;
import org.dwcj.concern.HasClassName;
import org.dwcj.concern.HasHtml;
import org.dwcj.concern.HasProperty;
import org.dwcj.concern.HasStyle;
import org.dwcj.concern.HasText;
import org.dwcj.dispatcher.EventListener;
import org.dwcj.dispatcher.ListenerRegistration;
import org.dwcj.exceptions.DwcjRuntimeException;

/**
 * Base class for a {@link Component} that represents a single built-in HTML element.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
public abstract class HtmlComponent<T extends HtmlComponent<T>> extends ElementComposite implements
    HasStyle<T>, HasAttribute<T>, HasProperty<T>, HasClassName<T>, HasText<T>, HasHtml<T> {

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
   * {@inheritDoc}
   */
  @Override
  public T addClassName(String className) {
    getElement().addClassName(className);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T removeClassName(String className) {
    getElement().removeClassName(className);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T setProperty(String property, Object value) {
    getElement().setProperty(property, value);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public <V> V getProperty(String property, Type typeOfV) {
    return getElement().getProperty(property, typeOfV);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T setAttribute(String attribute, String value) {
    getElement().setAttribute(attribute, value);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getAttribute(String attribute) {
    return getElement().getAttribute(attribute);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T removeAttribute(String attribute) {
    getElement().removeAttribute(attribute);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getStyle(String property) {
    return getElement().getStyle(property);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getComputedStyle(String property) {
    return getElement().getComputedStyle(property);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T setStyle(String property, String value) {
    getElement().setStyle(property, value);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T removeStyle(String property) {
    getElement().removeStyle(property);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getHtml() {
    return getElement().getHtml();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T setHtml(String html) {
    getElement().setHtml(html);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T setText(String text) {
    getElement().setText(text);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getText() {
    return getElement().getText();
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
   * @throws DwcjRuntimeException if the event class is not annotated with {@link EventName}
   * @throws IllegalStateException if the event name is already registered with a different event
   *         class
   */
  @Override
  public <E extends ComponentEvent<?>> ListenerRegistration<E> addEventListener(Class<E> eventClass,
      EventListener<E> listener, ElementEventOptions options) {
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
   * @throws DwcjRuntimeException if the event class is not annotated with {@link EventName}
   */
  @Override
  public <E extends ComponentEvent<?>> ListenerRegistration<E> addEventListener(Class<E> eventClass,
      EventListener<E> listener) {
    return super.addEventListener(eventClass, listener);
  }

  /**
   * Add a click listener to this component.
   *
   * @param listener the listener to add
   * @param options the options
   *
   * @return A listener registration for removing the event listener
   */
  public ListenerRegistration<HtmlClickEvent> addClickListener(
      EventListener<HtmlClickEvent> listener, ElementEventOptions options) {
    return addEventListener(HtmlClickEvent.class, listener, options);
  }

  /**
   * Add a click listener to this component.
   *
   * @param listener the listener to add
   *
   * @return A listener registration for removing the event listener
   */
  public ListenerRegistration<HtmlClickEvent> addClickListener(
      EventListener<HtmlClickEvent> listener) {
    return addEventListener(HtmlClickEvent.class, listener);
  }

  /**
   * Returns an instance of the current class, casted to its generic type. This method is primarily
   * used for method chaining in subclasses of HtmlComponent.
   *
   * @return An instance of the current class, casted to its generic type.
   */
  protected final T getSelf() {
    @SuppressWarnings("unchecked")
    T self = (T) this;

    return self;
  }
}
