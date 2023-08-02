package org.dwcj.component.button;

import com.basis.bbj.proxies.sysgui.BBjButton;
import com.basis.startup.type.BBjException;
import java.util.Arrays;
import java.util.List;
import org.dwcj.annotation.ExcludeFromJacocoGeneratedReport;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.Expanse;
import org.dwcj.component.button.event.ButtonClickEvent;
import org.dwcj.component.button.sink.ButtonClickEventSink;
import org.dwcj.component.event.BlurEvent;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.EventListener;
import org.dwcj.component.event.FocusEvent;
import org.dwcj.component.event.MouseEnterEvent;
import org.dwcj.component.event.MouseExitEvent;
import org.dwcj.component.event.RightMouseDownEvent;
import org.dwcj.component.event.sink.BlurEventSink;
import org.dwcj.component.event.sink.EventSinkListenerRegistry;
import org.dwcj.component.event.sink.FocusEventSink;
import org.dwcj.component.event.sink.MouseEnterEventSink;
import org.dwcj.component.event.sink.MouseExitEventSink;
import org.dwcj.component.event.sink.RightMouseDownEventSink;
import org.dwcj.concern.HasEnable;
import org.dwcj.concern.HasExpanse;
import org.dwcj.concern.HasFocus;
import org.dwcj.concern.HasHorizontalAlignment;
import org.dwcj.concern.HasTabTraversal;
import org.dwcj.concern.HasTheme;
import org.dwcj.exceptions.DwcjRuntimeException;

/**
 * An abstract button component implementation of {@link DwcButton}.
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
abstract class AbstractButton<T extends AbstractDwcComponent & HasFocus & HasTabTraversal & HasEnable>
    extends AbstractDwcComponent implements HasFocus, HasTabTraversal, HasEnable,
    HasExpanse<T, Expanse>, HasTheme<T, ButtonTheme>, HasHorizontalAlignment<T> {
  private boolean disableOnClick = false;

  private EventDispatcher dispatcher = new EventDispatcher();
  private EventSinkListenerRegistry<ButtonClickEvent> clickEventSinkListenerRegistry =
      new EventSinkListenerRegistry<>(new ButtonClickEventSink(this, dispatcher),
          ButtonClickEvent.class);
  private EventSinkListenerRegistry<FocusEvent> focusEventSinkListenerRegistry =
      new EventSinkListenerRegistry<>(new FocusEventSink(this, dispatcher), FocusEvent.class);
  private EventSinkListenerRegistry<BlurEvent> blurEventSinkListenerRegistry =
      new EventSinkListenerRegistry<>(new BlurEventSink(this, dispatcher), BlurEvent.class);
  private EventSinkListenerRegistry<MouseEnterEvent> mouseEnterEventSinkListenerRegistry =
      new EventSinkListenerRegistry<>(new MouseEnterEventSink(this, dispatcher),
          MouseEnterEvent.class);
  private EventSinkListenerRegistry<MouseExitEvent> mouseExitEventSinkListenerRegistry =
      new EventSinkListenerRegistry<>(new MouseExitEventSink(this, dispatcher),
          MouseExitEvent.class);
  private EventSinkListenerRegistry<RightMouseDownEvent> rightMouseDownEventSinkListenerRegistry =
      new EventSinkListenerRegistry<>(new RightMouseDownEventSink(this, dispatcher),
          RightMouseDownEvent.class);


  protected AbstractButton() {
    super();
    setExpanse(Expanse.MEDIUM);
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public T focus() {
    super.focusComponent();

    return getSelf();
  }

  /**
   * Check if the component has focus.
   *
   * <p>
   * The method will always reach the client to get the focus state. If the component is not
   * attached to a panel, the method will return false even if the component {@link #focus()} method
   * was called.
   * </p>
   *
   * @return true if the component has focus, false if not.
   */
  public boolean hasFocus() {
    return Boolean.valueOf(String.valueOf(getProperty("hasFocus")));
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public T setTabTraversable(Boolean traversable) {
    super.setComponentTabTraversable(traversable);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public Boolean isTabTraversable() {
    return super.isComponentTabTraversable();
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public T setEnabled(boolean enabled) {
    super.setComponentEnabled(enabled);

    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public boolean isEnabled() {
    return super.isComponentEnabled();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public T setExpanse(Expanse expanse) {
    setComponentExpanse(expanse);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Expanse getExpanse() {
    return (Expanse) getComponentExpanse();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public T setTheme(ButtonTheme theme) {
    setComponentTheme(theme);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public ButtonTheme getTheme() {
    return (ButtonTheme) getComponentTheme();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public T setHorizontalAlignment(Alignment alignment) {
    setComponentHorizontalAlignment(alignment);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Alignment getHorizontalAlignment() {
    return getComponentHorizontalAlignment();
  }

  /**
   * Set whether the button should be immediately disabled when the user clicks it.
   *
   * @param disableOnClick {@code true} to disable the button when the user clicks it
   *
   * @return the component itself
   */
  public T setDisableOnClick(boolean disableOnClick) {
    if (this.control != null) {
      try {
        ((BBjButton) control).setDisableOnClick(disableOnClick);
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }

    this.disableOnClick = disableOnClick;
    return getSelf();
  }

  /**
   * Returns whether the button is disabled when the user clicks it.
   *
   * @return {@code true} if the button is disabled when the user clicks it
   */
  public boolean isDisableOnClick() {
    if (this.control != null) {
      try {
        ((BBjButton) control).getDisableOnClick();
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }

    return this.disableOnClick;
  }

  /**
   * Sets the button name which used for accessibility.
   *
   * <p>
   * The name is used for accessibility. When the name is not explicitly set, the label will be used
   * by the browser instead. This is useful when the label of the button is only an icon, for
   * instance.
   * </p>
   *
   * @param name the name of the button
   * @return the component itself
   */
  public T setName(String name) {
    setUnrestrictedAttribute("name", name);
    return getSelf();
  }

  /**
   * Gets the button name.
   *
   * @return the name of the button
   * @see #setName(String)
   */
  public String getName() {
    String name = getAttribute("name");
    return name == null ? "" : name;
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public T setText(String text) {
    super.setText(text);

    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public T setVisible(Boolean visible) {
    super.setVisible(visible);

    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public T setTooltipText(String text) {
    super.setTooltipText(text);

    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public T setAttribute(String attribute, String value) {
    super.setAttribute(attribute, value);

    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public T setProperty(String property, Object value) {
    super.setProperty(property, value);

    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public T setStyle(String property, String value) {
    super.setStyle(property, value);

    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public T addClassName(String selector) {
    super.addClassName(selector);

    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public T removeClassName(String selector) {
    super.removeClassName(selector);
    return getSelf();
  }

  /**
   * Get the event dispatcher instance for the component.
   *
   * @return The instance of the event dispatcher.
   */
  EventDispatcher getEventDispatcher() {
    return this.dispatcher;
  }

  /**
   * Add a {@link ButtonClickEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return The component itself
   */
  public T addClickListener(EventListener<ButtonClickEvent> listener) {
    this.clickEventSinkListenerRegistry.addEventListener(listener);

    return getSelf();
  }

  /**
   * Alias for {@link #addClickListener(EventListener) addButtonClickListener}.
   *
   * @param listener the event listener to be added
   * @return The component itself
   *
   * @see AbstractOptionInputTest#addClickListener(EventListener)
   */
  public T onClick(EventListener<ButtonClickEvent> listener) {
    return addClickListener(listener);
  }

  /**
   * Removes a {@link ButtonClickEvent} listener from the component.
   *
   * @param listener the event listener to be removed
   * @return The component itself
   */
  public T removeClickListener(EventListener<ButtonClickEvent> listener) {
    this.clickEventSinkListenerRegistry.removeEventListener(listener);

    return getSelf();
  }

  /**
   * Add a {@link FocusEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return The component itself
   */
  public T addFocusListener(EventListener<FocusEvent> listener) {
    this.focusEventSinkListenerRegistry.addEventListener(listener);

    return getSelf();
  }

  /**
   * Alias for {@link #addFocusListener(EventListener) addFocusListener}.
   *
   * @param listener the event listener to be added
   * @return The component itself
   *
   * @see AbstractOptionInputTest#addFocusListener(EventListener)
   */
  public T onFocus(EventListener<FocusEvent> listener) {
    return addFocusListener(listener);
  }

  /**
   * Removes a {@link FocusEvent} listener from the component.
   *
   * @param listener the event listener to be removed
   * @return The component itself
   */
  public T removeFocusListener(EventListener<FocusEvent> listener) {
    this.focusEventSinkListenerRegistry.removeEventListener(listener);

    return getSelf();
  }

  /**
   * Add a {@link BlurEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return The component itself
   */
  public T addBlurListener(EventListener<BlurEvent> listener) {
    this.blurEventSinkListenerRegistry.addEventListener(listener);

    return getSelf();
  }

  /**
   * Alias for {@link #addBlurListener(EventListener) addBlurListener}.
   *
   * @param listener the event listener to be added
   * @return The component itself
   *
   * @see AbstractOptionInputTest#addBlurListener(EventListener)
   */
  public T onBlur(EventListener<BlurEvent> listener) {
    return addBlurListener(listener);
  }

  /**
   * Removes a {@link BlurEvent} listener from the component.
   *
   * @param listener the event listener to be removed
   * @return The component itself
   */
  public T removeBlurListener(EventListener<BlurEvent> listener) {
    this.blurEventSinkListenerRegistry.removeEventListener(listener);

    return getSelf();
  }

  /**
   * Adds a {@link MouseEnterEvent} for the component.
   *
   * @param listener the event listener to be added
   * @return The component itself
   */
  public T addMouseEnterListener(EventListener<MouseEnterEvent> listener) {
    this.mouseEnterEventSinkListenerRegistry.addEventListener(listener);

    return getSelf();
  }

  /**
   * Alias for {@link #addMouseEnterListener(EventListener) addMouseEnterListener}.
   *
   * @param listener the event listener to be added
   * @return The component itself
   *
   * @see AbstractOptionInputTest#addMouseEnterListener(EventListener)
   */
  public T onMouseEnter(EventListener<MouseEnterEvent> listener) {
    return addMouseEnterListener(listener);
  }

  /**
   * Remove a {@link MouseEnterEvent} listener from the component.
   *
   * @param listener the event listener to be removed
   * @return The component itself
   */
  public T removeMouseEnterListener(EventListener<MouseEnterEvent> listener) {
    this.mouseEnterEventSinkListenerRegistry.removeEventListener(listener);

    return getSelf();
  }

  /**
   * Add a {@link MouseExitEvent} for the component.
   *
   * @param listener the event listener to be added
   * @return The component itself
   */
  public T addMouseExitListener(EventListener<MouseExitEvent> listener) {
    this.mouseExitEventSinkListenerRegistry.addEventListener(listener);

    return getSelf();
  }

  /**
   * Alias for {@link #addMouseExitListener(EventListener) addMouseExitListener}.
   *
   * @param listener the event listener to be added
   * @return The component itself
   *
   * @see AbstractOptionInputTest#addMouseExitListener(EventListener)
   */
  public T onMouseExit(EventListener<MouseExitEvent> listener) {
    return addMouseExitListener(listener);
  }

  /**
   * Remove a {@link MouseExitEvent} listener from the component.
   *
   * @param listener the event listener to be removed
   * @return The component itself
   */
  public T removeMouseExitListener(EventListener<MouseExitEvent> listener) {
    this.mouseExitEventSinkListenerRegistry.removeEventListener(listener);

    return getSelf();
  }

  /**
   * Add a {@link RightMouseDownEvent} for the component.
   *
   * @param listener the event listener to be added
   * @return The component itself
   */
  public T addRightMouseDownListener(EventListener<RightMouseDownEvent> listener) {
    this.rightMouseDownEventSinkListenerRegistry.addEventListener(listener);

    return getSelf();
  }

  /**
   * Alias for {@link #addRightMouseDownListener(EventListener) addRightMouseDownListener}.
   *
   * @param listener the event listener to be added
   * @return The component itself
   *
   * @see AbstractOptionInputTest#addRightMouseDownListener(EventListener)
   */
  public T onRightMouseDown(EventListener<RightMouseDownEvent> listener) {
    return addRightMouseDownListener(listener);
  }

  /**
   * Remove a {@link RightMouseDownEvent} listener from the component.
   *
   * @param listener the event listener to be removed
   * @return The component itself
   */
  public T removeRightMouseDownListener(EventListener<RightMouseDownEvent> listener) {
    this.rightMouseDownEventSinkListenerRegistry.removeEventListener(listener);

    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<String> getRestrictedProperties() {
    List<String> properties = super.getRestrictedProperties();
    properties.addAll(Arrays.asList("autofocus", "disabled", "distance", "dropdownButtonWidth",
        "expanse", "hasFocus", "label", "name", "selected", "skidding", "tabTraversable", "theme",
        "toggleable", "value"));

    return properties;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void catchUp() throws IllegalAccessException {
    super.catchUp();

    // catch up the event listeners
    clickEventSinkListenerRegistry.catchUp();
    focusEventSinkListenerRegistry.catchUp();
    blurEventSinkListenerRegistry.catchUp();
    mouseEnterEventSinkListenerRegistry.catchUp();
    mouseExitEventSinkListenerRegistry.catchUp();
    rightMouseDownEventSinkListenerRegistry.catchUp();

    if (Boolean.TRUE.equals(disableOnClick)) {
      setDisableOnClick(disableOnClick);
    }
  }

  private T getSelf() {
    @SuppressWarnings("unchecked")
    T self = (T) this;

    return self;
  }
}
