package com.webforj.component.html;

import com.webforj.component.Component;
import com.webforj.component.element.Element;
import com.webforj.component.element.ElementComposite;
import com.webforj.component.element.annotation.EventName;
import com.webforj.component.element.event.ElementEventOptions;
import com.webforj.component.event.ComponentEvent;
import com.webforj.component.html.event.HtmlClickEvent;
import com.webforj.concern.HasAttribute;
import com.webforj.concern.HasClassName;
import com.webforj.concern.HasHtml;
import com.webforj.concern.HasProperty;
import com.webforj.concern.HasStyle;
import com.webforj.concern.HasText;
import com.webforj.concern.HasVisibility;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.exceptions.DwcjRuntimeException;
import java.lang.reflect.Type;

/**
 * Base class for a {@link Component} that represents a single built-in HTML element.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
public abstract class HtmlComponent<T extends HtmlComponent<T>> extends ElementComposite
    implements HasStyle<T>, HasAttribute<T>, HasProperty<T>, HasClassName<T>, HasText<T>,
    HasHtml<T>, HasVisibility<T> {

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
  public T addClassName(String... className) {
    getElement().addClassName(className);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T removeClassName(String... className) {
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
   * {@inheritDoc}
   */
  @Override
  public T setVisible(boolean visible) {
    getElement().setVisible(visible);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isVisible() {
    return getElement().isVisible();
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
   * @throws DwcjRuntimeException if the event class is not annotated with {@link EventName}
   */
  @Override
  public <E extends ComponentEvent<?>> ListenerRegistration<E> addEventListener(
      Class<? super E> eventClass, EventListener<E> listener) {
    return super.addEventListener(eventClass, listener);
  }

  /**
   * Adds a click listener to this component.
   *
   * @param listener the listener to add
   * @param options the options
   *
   * @return A listener registration for removing the event listener
   */
  public ListenerRegistration<HtmlClickEvent<T>> addClickListener(
      EventListener<HtmlClickEvent<T>> listener, ElementEventOptions options) {
    return addEventListener(HtmlClickEvent.class, listener, options);
  }

  /**
   * Adds a click listener to this component.
   *
   * @param listener the listener to add
   *
   * @return A listener registration for removing the event listener
   */
  public ListenerRegistration<HtmlClickEvent<T>> addClickListener(
      EventListener<HtmlClickEvent<T>> listener) {
    return addEventListener(HtmlClickEvent.class, listener);
  }

  /**
   * Adds an onClick listener to this component.
   *
   * @param listener the listener to add
   * @param options the options
   *
   * @return A listener registration for removing the event listener
   */
  public ListenerRegistration<HtmlClickEvent<T>> onClick(EventListener<HtmlClickEvent<T>> listener,
      ElementEventOptions options) {
    return addClickListener(listener, options);
  }

  /**
   * Adds an onClick listener to this component.
   *
   * @param listener the listener to add
   *
   * @return A listener registration for removing the event listener
   */
  public ListenerRegistration<HtmlClickEvent<T>> onClick(
      EventListener<HtmlClickEvent<T>> listener) {
    return addClickListener(listener);
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
