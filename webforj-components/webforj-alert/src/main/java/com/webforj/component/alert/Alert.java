package com.webforj.component.alert;

import com.webforj.component.Component;
import com.webforj.component.Expanse;
import com.webforj.component.Theme;
import com.webforj.component.alert.event.AlertCloseEvent;
import com.webforj.component.element.Element;
import com.webforj.component.element.ElementCompositeContainer;
import com.webforj.component.element.PropertyDescriptor;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.element.annotation.PropertyExclude;
import com.webforj.component.element.annotation.PropertyMethods;
import com.webforj.concern.HasAttribute;
import com.webforj.concern.HasClassName;
import com.webforj.concern.HasHtml;
import com.webforj.concern.HasSize;
import com.webforj.concern.HasStyle;
import com.webforj.concern.HasText;
import com.webforj.concern.HasVisibility;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;

/**
 * The alert component provides contextual feedback messages for the user.
 *
 * @author Hyyan Abo Fakher
 * @since 25.00
 */
@NodeName("dwc-alert")
public class Alert extends ElementCompositeContainer
    implements HasText<Alert>, HasHtml<Alert>, HasClassName<Alert>, HasStyle<Alert>,
    HasVisibility<Alert>, HasSize<Alert>, HasAttribute<Alert> {

  // Property descriptors
  private final PropertyDescriptor<Boolean> closableProp =
      PropertyDescriptor.property("closable", false);
  private final PropertyDescriptor<Expanse> expanseProp =
      PropertyDescriptor.property("expanse", Expanse.MEDIUM);
  private final PropertyDescriptor<Theme> themeProp =
      PropertyDescriptor.property("theme", Theme.DEFAULT);
  @PropertyExclude
  private final PropertyDescriptor<Boolean> openedProp =
      PropertyDescriptor.property("opened", true);

  @PropertyMethods(setter = "setText", getter = "getText")
  private final PropertyDescriptor<String> messageProp = PropertyDescriptor.property("message", "");

  /**
   * Constructs a new Alert with the given text, theme, and closable property.
   *
   * @param text the text message for the alert
   * @param theme the theme of the alert
   * @param closable whether the alert is closable
   */
  public Alert(String text, Theme theme, boolean closable) {
    this.setText(text);
    this.setTheme(theme);
    this.setClosable(closable);
  }

  /**
   * Constructs a new Alert with the given text and theme.
   *
   * @param text the text message for the alert
   * @param theme the theme of the alert
   */
  public Alert(String text, Theme theme) {
    this(text, theme, false);
  }

  /**
   * Constructs a new Alert with the given text.
   *
   * @param text the text message for the alert
   */
  public Alert(String text) {
    this(text, Theme.DEFAULT);
  }

  /**
   * Constructs a new Alert.
   */
  public Alert() {
    this("");
  }

  /**
   * Alias for {@link #add(Component...)}.
   *
   * @param components the component to be added
   * @return the component itself
   */
  public Alert addToContent(Component... components) {
    getElement().add(components);
    return this;
  }

  /**
   * Sets the alert closable.
   *
   * <p>
   * When true, the alert can be closed by the user. The default value is false. Note that this will
   * just hide the alert, it will not remove it from the DOM or destroy the used component instance.
   * To completely remove the alert from the DOM, you need to call {@link Alert#destroy()}.
   * </p>
   *
   * @param closable {@code true} if the alert is closable, {@code false} otherwise
   * @return the component itself
   */
  public Alert setClosable(boolean closable) {
    set(closableProp, closable);
    return this;
  }

  /**
   * Checks if the alert is closable.
   *
   * @return the closable
   */
  public boolean isClosable() {
    return get(closableProp);
  }

  /**
   * Sets the alert expanse.
   *
   * @param expanse the expanse
   * @return the component itself
   */
  public Alert setExpanse(Expanse expanse) {
    set(expanseProp, expanse);
    return this;
  }

  /**
   * Gets the alert expanse.
   *
   * @return the expanse
   */
  public Expanse getExpanse() {
    return get(expanseProp);
  }

  /**
   * Opens the alert.
   *
   * @return the component itself
   */
  public Alert open() {
    set(openedProp, true);
    return this;
  }

  /**
   * Closes the alert.
   *
   * @return the component itself
   */
  public Alert close() {
    set(openedProp, false);
    return this;
  }

  /**
   * Checks if the alert is opened.
   *
   * @return {@code true} if the alert is opened, {@code false} otherwise
   */
  public boolean isOpened() {
    if (!isClosable()) {
      return get(openedProp);
    } else {
      // must reach to the client side to check if the alert is opened
      return get(openedProp, true, Boolean.class);
    }
  }

  /**
   * Sets the alert theme.
   *
   * @param theme the theme
   * @return the component itself
   */
  public Alert setTheme(Theme theme) {
    set(themeProp, theme);
    return this;
  }

  /**
   * Gets the alert theme.
   *
   * @return the theme
   */
  public Theme getTheme() {
    return get(themeProp);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Alert setText(String text) {
    setHtml(sanitizeHtml(text));
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getText() {
    return sanitizeHtml(getHtml());
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Alert setVisible(boolean visible) {
    return visible ? open() : close();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isVisible() {
    return isOpened();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Alert setHtml(String html) {
    set(messageProp, html);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getHtml() {
    return get(messageProp);
  }

  /**
   * Adds a listener for the alert close event.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<AlertCloseEvent> addCloseListener(
      EventListener<AlertCloseEvent> listener) {
    return addEventListener(AlertCloseEvent.class, listener);
  }

  /**
   * Alias for {@link #addCloseListener(EventListener)}.
   *
   * @param listener the listener
   * @return A registration object for removing the event listener
   * @see #addCloseListener(EventListener)
   */
  public ListenerRegistration<AlertCloseEvent> onClose(EventListener<AlertCloseEvent> listener) {
    return addCloseListener(listener);
  }

  private String sanitizeHtml(String html) {
    return html.replaceAll("\\<[^>]*>", "");
  }

  Element getOriginalElement() {
    return getElement();
  }
}
