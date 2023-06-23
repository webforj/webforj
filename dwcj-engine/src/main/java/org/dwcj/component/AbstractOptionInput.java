package org.dwcj.component;

import com.basis.bbj.proxies.sysgui.BBjToggleButton;
import com.basis.startup.type.BBjException;
import org.dwcj.annotation.ExcludeFromJacocoGeneratedReport;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.component.event.BlurEvent;
import org.dwcj.component.event.CheckedEvent;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.EventListener;
import org.dwcj.component.event.FocusEvent;
import org.dwcj.component.event.MouseEnterEvent;
import org.dwcj.component.event.MouseExitEvent;
import org.dwcj.component.event.RightMouseDownEvent;
import org.dwcj.component.event.ToggleEvent;
import org.dwcj.component.event.UncheckedEvent;
import org.dwcj.component.event.sink.BlurEventSink;
import org.dwcj.component.event.sink.CheckedEventSink;
import org.dwcj.component.event.sink.EventSinkListenerRegistry;
import org.dwcj.component.event.sink.FocusEventSink;
import org.dwcj.component.event.sink.MouseEnterEventSink;
import org.dwcj.component.event.sink.MouseExitEventSink;
import org.dwcj.component.event.sink.RightMouseDownEventSink;
import org.dwcj.component.event.sink.ToggleEventSink;
import org.dwcj.component.event.sink.UncheckedEventSink;
import org.dwcj.exceptions.DwcjRuntimeException;

/**
 * The AbstractOptionInput class is an abstract base class for input components that represent
 * options, such as checkboxes and radio buttons. It extends the AbstractDwcComponent class and
 * implements the HasFocus, TabTraversable, TextPosition, and HasEnable interfaces.
 *
 * @param <T> the concrete subclass that extends AbstractOptionInput
 *
 * @author Hyyan Abo Fakher
 * @since 23.01
 */
public abstract class AbstractOptionInput<T extends AbstractDwcComponent & HasFocus & TabTraversable & TextPosition & HasEnable>
    extends AbstractDwcComponent implements HasFocus, TabTraversable, TextPosition, HasEnable {

  /**
   * Expanse options for the component.
   */
  public enum Expanse {
    /* The xlarge expanse as defined by the BBj standard */
    XLARGE("xl"),
    /* The large expanse as defined by the BBj standard */
    LARGE("l"),
    /* The medium expanse as defined by the BBj standard */
    MEDIUM("m"),
    /* The small expanse as defined by the BBj standard */
    SMALL("s"),
    /* The xsmall expanse as defined by the BBj standard */
    XSMALL("xs");

    private final String value;

    Expanse(String value) {
      this.value = value;
    }

    /**
     * Get the value of the expanse.
     *
     * @return The value of the expanse.
     */
    public String getValue() {
      return this.value;
    }
  }

  private TextPosition.Position textPosition = TextPosition.Position.RIGHT;
  private Boolean checked = null;
  private Expanse expanse;

  private EventDispatcher dispatcher = new EventDispatcher();
  private EventSinkListenerRegistry<CheckedEvent> checkEventSinkListenerRegistry =
      new EventSinkListenerRegistry<>(new CheckedEventSink(this, dispatcher), CheckedEvent.class);
  private EventSinkListenerRegistry<UncheckedEvent> uncheckEventSinkListenerRegistry =
      new EventSinkListenerRegistry<>(new UncheckedEventSink(this, dispatcher),
          UncheckedEvent.class);
  private EventSinkListenerRegistry<ToggleEvent> toggleEventSinkListenerRegistry =
      new EventSinkListenerRegistry<>(new ToggleEventSink(this, dispatcher), ToggleEvent.class);
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

  /**
   * Create a new AbstractOptionInput component.
   *
   * @param text Desired text for the AbstractOptionInput.
   * @param checked True if the AbstractOptionInput should be created as checked, false otherwise.
   */
  protected AbstractOptionInput(String text, boolean checked) {
    super();
    this.setText(text);
    this.checked = checked;
  }

  /**
   * Create a new AbstractOptionInput component.
   *
   * @param text The text for the AbstractOptionInput.
   */
  protected AbstractOptionInput(String text) {
    this(text, false);
  }

  /**
   * Create a new AbstractOptionInput component.
   */
  protected AbstractOptionInput() {
    this("", false);
  }

  /**
   * Checks or unchecks the component.
   *
   * @param checked true if checked, false if unchecked.
   *
   * @return The component itself
   */
  public T setChecked(boolean checked) {
    BBjToggleButton theControl = getBbjControl();

    if (theControl != null) {
      try {
        theControl.setSelected(checked);
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }

    this.checked = checked;

    return getSelf();
  }

  /**
   * Checks if the component is checked.
   *
   * @return false if not checked, true if checked.
   */
  public boolean isChecked() {
    BBjToggleButton theControl = getBbjControl();

    if (theControl != null) {
      try {
        return theControl.isSelected();
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }

    return this.checked;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T setTextPosition(Position position) {
    BBjToggleButton theControl = getBbjControl();

    if (theControl != null) {
      try {
        theControl.setHorizontalTextPosition(0);
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }

    this.textPosition = position;

    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Position getTextPosition() {
    return this.textPosition;
  }

  /**
   * Sets the expanse for the component.
   *
   * @param expanse The component expanse
   * @return The component itself
   */
  public T setExpanse(Expanse expanse) {
    this.expanse = expanse;
    setProperty("expanse", expanse.getValue());

    return getSelf();
  }

  /**
   * Get the expanse of the component.
   *
   * @return The expanse for the component.
   */
  public Expanse getExpanse() {
    return this.expanse;
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
   * Add a {@link CheckedEvent} listener to the component.
   *
   * @param listener the event listener to be added
   * @return The component itself
   */
  public T addCheckListener(EventListener<CheckedEvent> listener) {
    this.checkEventSinkListenerRegistry.addEventListener(listener);
    return getSelf();
  }

  /**
   * Alias for {@link #addCheckListener(EventListener) addCheckedListener}.
   *
   * @param listener the event listener to be added
   * @return The component itself
   *
   * @see AbstractOptionInputTest#addCheckListener(EventListener)
   */
  public T onCheck(EventListener<CheckedEvent> listener) {
    return addCheckListener(listener);
  }

  /**
   * Remove a {@link CheckedEvent} listener from the component.
   *
   * @param listener the event listener to be removed
   * @return The component itself
   */
  public T removeCheckListener(EventListener<CheckedEvent> listener) {
    this.checkEventSinkListenerRegistry.removeEventListener(listener);
    return getSelf();
  }

  /**
   * Add an {@link UncheckedEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return The component itself
   */
  public T addUncheckListener(EventListener<UncheckedEvent> listener) {
    this.uncheckEventSinkListenerRegistry.addEventListener(listener);

    return getSelf();
  }

  /**
   * Alias for {@link #addUncheckListener(EventListener) addUncheckedListener}.
   *
   * @param listener the event listener to be added
   * @return The component itself
   *
   * @see AbstractOptionInputTest#addUncheckListener(EventListener)
   */
  public T onUncheck(EventListener<UncheckedEvent> listener) {
    return addUncheckListener(listener);
  }

  /**
   * Remove an {@link UncheckedEvent} listener from the component.
   *
   * @param listener the event listener to be removed
   * @return The component itself
   */
  public T removeUncheckListener(EventListener<UncheckedEvent> listener) {
    this.uncheckEventSinkListenerRegistry.removeEventListener(listener);

    return getSelf();
  }

  /**
   * Add a {@link ToggleEvent} listener for the component.
   *
   * @param listener the event listener to be added
   * @return The component itself
   */
  public T addToggleListener(EventListener<ToggleEvent> listener) {
    this.toggleEventSinkListenerRegistry.addEventListener(listener);

    return getSelf();
  }

  /**
   * Alias for {@link #addToggleListener(EventListener) addToggleListener}.
   *
   * @param listener the event listener to be added
   * @return The component itself
   *
   * @see AbstractOptionInputTest#addToggleListener(EventListener)
   */
  public T onToggle(EventListener<ToggleEvent> listener) {
    return addToggleListener(listener);
  }

  /**
   * Remove a {@link ToggleEvent} listener from the component.
   *
   * @param listener the event listener to be removed
   * @return The component itself
   */
  public T removeToggleListener(EventListener<ToggleEvent> listener) {
    this.toggleEventSinkListenerRegistry.removeEventListener(listener);

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
  protected void catchUp() throws IllegalAccessException {
    if (Boolean.TRUE.equals(this.getCaughtUp())) {
      throw new IllegalAccessException("catchUp cannot be called twice");
    }

    super.catchUp();

    // catch up the event listeners
    this.checkEventSinkListenerRegistry.catchUp();
    this.uncheckEventSinkListenerRegistry.catchUp();
    this.toggleEventSinkListenerRegistry.catchUp();
    this.focusEventSinkListenerRegistry.catchUp();
    this.blurEventSinkListenerRegistry.catchUp();
    this.mouseEnterEventSinkListenerRegistry.catchUp();
    this.mouseExitEventSinkListenerRegistry.catchUp();
    this.rightMouseDownEventSinkListenerRegistry.catchUp();

    if (this.checked != null) {
      this.setChecked(this.checked);
    }

    if (this.textPosition != Position.RIGHT) {
      this.setTextPosition(this.textPosition);
    }
  }

  protected BBjToggleButton getBbjControl() {
    try {
      return (BBjToggleButton) ComponentAccessor.getDefault().getBBjControl(this);
    } catch (IllegalAccessException e) {
      throw new DwcjRuntimeException(e);
    }
  }

  private T getSelf() {
    @SuppressWarnings("unchecked")
    T self = (T) this;

    return self;
  }
}

