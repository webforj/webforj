package org.dwcj.component.checkbox;

import org.dwcj.annotation.ExcludeFromJacocoGeneratedReport;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.HasEnable;
import org.dwcj.component.HasFocus;
import org.dwcj.component.TabTraversable;
import org.dwcj.component.TextPosition;
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
import org.dwcj.component.event.sink.EventSinkManager;
import org.dwcj.component.event.sink.FocusEventSink;
import org.dwcj.component.event.sink.MouseEnterEventSink;
import org.dwcj.component.event.sink.MouseExitEventSink;
import org.dwcj.component.event.sink.RightMouseDownEventSink;
import org.dwcj.component.event.sink.ToggleEventSink;
import org.dwcj.component.event.sink.UncheckedEventSink;
import org.dwcj.component.window.AbstractWindow;
import org.dwcj.exceptions.DwcjRuntimeException;
import org.dwcj.utilities.BBjFunctionalityHelper;

import com.basis.bbj.proxies.sysgui.BBjCheckBox;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;

/**
 * A Checkbox component.
 */
public final class CheckBox extends AbstractDwcComponent
    implements HasFocus, TabTraversable, TextPosition, HasEnable {

  /**
   * Expanse options for the checkbox.
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

  private EventDispatcher dispatcher = new EventDispatcher();
  private EventSinkManager<CheckedEvent> checkedEventSinkManager =
      new EventSinkManager<>(new CheckedEventSink(this, dispatcher), CheckedEvent.class);
  private EventSinkManager<UncheckedEvent> unCheckedEventSinkManager =
      new EventSinkManager<>(new UncheckedEventSink(this, dispatcher), UncheckedEvent.class);
  private EventSinkManager<ToggleEvent> toggleEventSinkManager =
      new EventSinkManager<>(new ToggleEventSink(this, dispatcher), ToggleEvent.class);
  private EventSinkManager<FocusEvent> focusEventSinkManager =
      new EventSinkManager<>(new FocusEventSink(this, dispatcher), FocusEvent.class);
  private EventSinkManager<BlurEvent> blurEventSinkManager =
      new EventSinkManager<>(new BlurEventSink(this, dispatcher), BlurEvent.class);
  private EventSinkManager<MouseEnterEvent> mouseEnterEventSinkManager =
      new EventSinkManager<>(new MouseEnterEventSink(this, dispatcher), MouseEnterEvent.class);
  private EventSinkManager<MouseExitEvent> mouseExitEventSinkManager =
      new EventSinkManager<>(new MouseExitEventSink(this, dispatcher), MouseExitEvent.class);
  private EventSinkManager<RightMouseDownEvent> rightMouseDownEventSinkManager =
      new EventSinkManager<>(new RightMouseDownEventSink(this, dispatcher),
          RightMouseDownEvent.class);

  private TextPosition.Position textPosition = TextPosition.Position.RIGHT;
  private Boolean checked = null;
  private boolean indeterminate = false;
  private Expanse expanse;

  /**
   * Create a new checkbox component.
   */
  public CheckBox(String text, boolean checked) {
    super();
    this.setText(text);
    this.checked = checked;
  }

  /**
   * Create a new checkbox component.
   *
   * @param text The text for the checkbox.
   */
  public CheckBox(String text) {
    this(text, false);
  }

  /**
   * Create a new checkbox component.
   */
  public CheckBox() {
    this("", false);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void create(AbstractWindow p) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(p);
      byte[] flags =
          BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
      setControl(w.addCheckBox("", flags));
      this.catchUp();
    } catch (IllegalAccessException | BBjException e) {
      throw new DwcjRuntimeException("Failed to create the BBjCheckBox Control", e);
    }
  }

  /**
   * Add a {@link CheckedEvent} listener to the checkbox component.
   *
   * @param listener the event listener to be added
   * @return The checkbox itself
   */
  public CheckBox addCheckedListener(EventListener<CheckedEvent> listener) {
    this.checkedEventSinkManager.addEventListener(listener);
    return this;
  }

  /**
   * Alias for {@link #addCheckedListener(EventListener) addCheckedListener}.
   *
   * @param listener the event listener to be added
   * @return The checkbox itself
   *
   * @see Checkbox#addCheckedListener(EventListener)
   */
  public CheckBox onChecked(EventListener<CheckedEvent> listener) {
    return addCheckedListener(listener);
  }

  /**
   * Remove a {@link CheckedEvent} listener from the checkbox component.
   *
   * @param listener the event listener to be removed
   * @return The checkbox itself
   */
  public CheckBox removeCheckedListener(EventListener<CheckedEvent> listener) {
    this.checkedEventSinkManager.removeEventListener(listener);
    return this;
  }

  /**
   * Add an {@link UncheckedEvent} listener for the checkbox component.
   *
   * @param listener the event listener to be added
   * @return The checkbox itself
   */
  public CheckBox addUncheckedListener(EventListener<UncheckedEvent> listener) {
    this.unCheckedEventSinkManager.addEventListener(listener);
    return this;
  }

  /**
   * Alias for {@link #addUncheckedListener(EventListener) addUncheckedListener}.
   *
   * @param listener the event listener to be added
   * @return The checkbox itself
   *
   * @see Checkbox#addUncheckedListener(EventListener)
   */
  public CheckBox onUnchecked(EventListener<UncheckedEvent> listener) {
    return addUncheckedListener(listener);
  }

  /**
   * Remove an {@link UncheckedEvent} listener from the checkbox component.
   *
   * @param listener the event listener to be removed
   * @return The checkbox itself
   */
  public CheckBox removeUncheckedListener(EventListener<UncheckedEvent> listener) {
    this.unCheckedEventSinkManager.removeEventListener(listener);
    return this;
  }

  /**
   * Add a {@link ToggleEvent} listener for the checkbox component.
   *
   * @param listener the event listener to be added
   * @return The checkbox itself
   */
  public CheckBox addToggleListener(EventListener<ToggleEvent> listener) {
    this.toggleEventSinkManager.addEventListener(listener);
    return this;
  }

  /**
   * Alias for {@link #addToggleListener(EventListener) addToggleListener}.
   *
   * @param listener the event listener to be added
   * @return The checkbox itself
   *
   * @see Checkbox#addToggleListener(EventListener)
   */
  public CheckBox onToggle(EventListener<ToggleEvent> listener) {
    return addToggleListener(listener);
  }

  /**
   * Remove a {@link ToggleEvent} listener from the checkbox component.
   *
   * @param listener the event listener to be removed
   * @return The checkbox itself
   */
  public CheckBox removeToggleListener(EventListener<ToggleEvent> listener) {
    this.toggleEventSinkManager.removeEventListener(listener);
    return this;
  }

  /**
   * Add a {@link FocusEvent} listener for the checkbox component.
   *
   * @param listener the event listener to be added
   * @return The checkbox itself
   */
  public CheckBox addFocusListener(EventListener<FocusEvent> listener) {
    this.focusEventSinkManager.addEventListener(listener);
    return this;
  }

  /**
   * Alias for {@link #addFocusListener(EventListener) addFocusListener}.
   *
   * @param listener the event listener to be added
   * @return The checkbox itself
   *
   * @see Checkbox#addFocusListener(EventListener)
   */
  public CheckBox onFocus(EventListener<FocusEvent> listener) {
    return addFocusListener(listener);
  }

  /**
   * Removes a {@link FocusEvent} listener from the checkbox component.
   *
   * @param listener the event listener to be removed
   * @return The checkbox itself
   */
  public CheckBox removeFocusListener(EventListener<FocusEvent> listener) {
    this.focusEventSinkManager.removeEventListener(listener);
    return this;
  }

  /**
   * Add a {@link BlurEvent} listener for the checkbox component.
   *
   * @param listener the event listener to be added
   * @return The checkbox itself
   */
  public CheckBox addBlurListener(EventListener<BlurEvent> listener) {
    this.blurEventSinkManager.addEventListener(listener);
    return this;
  }

  /**
   * Alias for {@link #addBlurListener(EventListener) addBlurListener}.
   *
   * @param listener the event listener to be added
   * @return The checkbox itself
   *
   * @see Checkbox#addBlurListener(EventListener)
   */
  public CheckBox onBlur(EventListener<BlurEvent> listener) {
    return addBlurListener(listener);
  }

  /**
   * Removes a {@link BlurEvent} listener from the checkbox component.
   *
   * @param listener the event listener to be removed
   * @return The checkbox itself
   */
  public CheckBox removeBlurListener(EventListener<BlurEvent> listener) {
    this.blurEventSinkManager.removeEventListener(listener);
    return this;
  }

  /**
   * Adds a {@link MouseEnterEvent} for the checkbox component.
   *
   * @param listener the event listener to be added
   * @return The checkbox itself
   */
  public CheckBox addMouseEnterListener(EventListener<MouseEnterEvent> listener) {
    this.mouseEnterEventSinkManager.addEventListener(listener);
    return this;
  }

  /**
   * Alias for {@link #addMouseEnterListener(EventListener) addMouseEnterListener}.
   *
   * @param listener the event listener to be added
   * @return The checkbox itself
   *
   * @see Checkbox#addMouseEnterListener(EventListener)
   */
  public CheckBox onMouseEnter(EventListener<MouseEnterEvent> listener) {
    return addMouseEnterListener(listener);
  }

  /**
   * Remove a {@link MouseEnterEvent} listener from the checkbox component.
   *
   * @param listener the event listener to be removed
   * @return The checkbox itself
   */
  public CheckBox removeMouseEnterListener(EventListener<MouseEnterEvent> listener) {
    this.mouseEnterEventSinkManager.removeEventListener(listener);
    return this;
  }

  /**
   * Add a {@link MouseExitEvent} for the checkbox component.
   *
   * @param listener the event listener to be added
   * @return The checkbox itself
   */
  public CheckBox addMouseExitListener(EventListener<MouseExitEvent> listener) {
    this.mouseExitEventSinkManager.addEventListener(listener);
    return this;
  }

  /**
   * Alias for {@link #addMouseExitListener(EventListener) addMouseExitListener}.
   *
   * @param listener the event listener to be added
   * @return The checkbox itself
   *
   * @see Checkbox#addMouseExitListener(EventListener)
   */
  public CheckBox onMouseExit(EventListener<MouseExitEvent> listener) {
    return addMouseExitListener(listener);
  }

  /**
   * Remove a {@link MouseExitEvent} listener from the checkbox component.
   *
   * @param listener the event listener to be removed
   * @return The checkbox itself
   */
  public CheckBox removeMouseExitListener(EventListener<MouseExitEvent> listener) {
    this.mouseExitEventSinkManager.removeEventListener(listener);
    return this;
  }

  /**
   * Add a {@link RightMouseDownEvent} for the checkbox component.
   *
   * @param listener the event listener to be added
   * @return The checkbox itself
   */
  public CheckBox addRightMouseDownListener(EventListener<RightMouseDownEvent> listener) {
    this.rightMouseDownEventSinkManager.addEventListener(listener);
    return this;
  }

  /**
   * Alias for {@link #addRightMouseDownListener(EventListener) addRightMouseDownListener}.
   *
   * @param listener the event listener to be added
   * @return The checkbox itself
   *
   * @see Checkbox#addRightMouseDownListener(EventListener)
   */
  public CheckBox onRightMouseDown(EventListener<RightMouseDownEvent> listener) {
    return addRightMouseDownListener(listener);
  }

  /**
   * Remove a {@link RightMouseDownEvent} listener from the checkbox component.
   *
   * @param listener the event listener to be removed
   * @return The checkbox itself
   */
  public CheckBox removeRightMouseDownListener(EventListener<RightMouseDownEvent> listener) {
    this.rightMouseDownEventSinkManager.removeEventListener(listener);
    return this;
  }

  /**
   * Checks or unchecks the checkbox.
   *
   * @param checked true if checked, false if unchecked.
   *
   * @return The checkbox itself
   */
  public CheckBox setChecked(boolean checked) {
    BBjCheckBox checkbox = getBBjControl();

    if (checkbox != null) {
      try {
        checkbox.setSelected(checked);
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }

    this.checked = checked;
    return this;
  }

  /**
   * Checks if the checkbox is checked.
   *
   * @return false if not checked, true if checked.
   */
  public boolean isChecked() {
    BBjCheckBox checkbox = getBBjControl();

    if (checkbox != null) {
      try {
        return checkbox.isSelected();
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }

    return this.checked;
  }

  /**
   * Set the checkbox to be indeterminate.
   *
   * @param value When true then the checkbox's value is neither true nor false, but is instead
   *        indeterminate, meaning that its state cannot be determined or stated in pure binary
   *        terms.
   *
   * @return The checkbox itself
   */
  public CheckBox setIndeterminate(boolean value) {
    setProperty("indeterminate", value);
    this.indeterminate = value;
    return this;
  }

  /**
   * Returns wether or not the checkbox is indeterminate.
   *
   * @return A Boolean representing wether or not the checkbox is indeterminate.
   */
  public boolean isIndeterminate() {
    return this.indeterminate;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public CheckBox setTextPosition(Position position) {
    BBjCheckBox checkbox = getBBjControl();

    if (checkbox != null) {
      try {
        checkbox.setHorizontalTextPosition(0);
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }

    this.textPosition = position;
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Position getTextPosition() {
    return this.textPosition;
  }

  /**
   * Sets the expanse for the checkbox.
   *
   * @param expanse The checkbox expanse
   * @return The checkbox itself
   */
  public CheckBox setExpanse(Expanse expanse) {
    this.expanse = expanse;
    setProperty("expanse", expanse.getValue());
    return this;
  }

  /**
   * Get the expanse of the checkbox.
   *
   * @return The expanse for the checkbox.
   */
  public Expanse getExpanse() {
    return this.expanse;
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public CheckBox focus() {
    super.focusComponent();
    return this;
  }

  /**
   * Check if the checkbox has focus.
   *
   * <p>
   * The method will always reach the client to get the focus state. If the checkbox is not attached
   * to a panel, the method will return false even if the checkbox {@link #focus()} method was
   * called.
   * </p>
   *
   * @return true if the checkbox has focus, false if not.
   */
  public boolean hasFocus() {
    return Boolean.valueOf(String.valueOf(getProperty("hasFocus")));
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public CheckBox setTabTraversable(Boolean traversable) {
    super.setComponentTabTraversable(traversable);
    return this;
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
  public CheckBox setText(String text) {
    super.setText(text);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public CheckBox setVisible(Boolean visible) {
    super.setVisible(visible);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public CheckBox setEnabled(boolean enabled) {
    super.setComponentEnabled(enabled);
    return this;
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
  public CheckBox setTooltipText(String text) {
    super.setTooltipText(text);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public CheckBox setAttribute(String attribute, String value) {
    super.setAttribute(attribute, value);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport

  public CheckBox setProperty(String property, Object value) {
    super.setProperty(property, value);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public CheckBox setStyle(String property, String value) {
    super.setStyle(property, value);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public CheckBox addClassName(String selector) {
    super.addClassName(selector);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public CheckBox removeClassName(String selector) {
    super.removeClassName(selector);
    return this;
  }

  /**
   * Get the event dispatcher instance for the checkbox.
   *
   * @return The instance of the event dispatcher.
   */
  EventDispatcher getEventDispatcher() {
    return this.dispatcher;
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

    if (this.checked != null) {
      this.setChecked(this.checked);
    }

    if (this.textPosition != Position.RIGHT) {
      this.setTextPosition(this.textPosition);
    }
  }

  private BBjCheckBox getBBjControl() {
    try {
      return (BBjCheckBox) ComponentAccessor.getDefault().getBBjControl(this);
    } catch (IllegalAccessException e) {
      throw new DwcjRuntimeException(e);
    }
  }
}
