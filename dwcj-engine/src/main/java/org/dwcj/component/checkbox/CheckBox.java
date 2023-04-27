package org.dwcj.component.checkbox;

import com.basis.bbj.proxies.sysgui.BBjCheckBox;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.Environment;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.Focusable;
import org.dwcj.component.HasEnable;
import org.dwcj.component.HasReadOnly;
import org.dwcj.component.TabTraversable;
import org.dwcj.component.TextAlignable;
import org.dwcj.component.event.BlurEvent;
import org.dwcj.component.event.CheckedEvent;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.EventListener;
import org.dwcj.component.event.FocusEvent;
import org.dwcj.component.event.MouseEnterEvent;
import org.dwcj.component.event.MouseExitEvent;
import org.dwcj.component.event.RightMouseDownEvent;
import org.dwcj.component.event.UncheckedEvent;
import org.dwcj.component.event.sink.BlurEventSink;
import org.dwcj.component.event.sink.CheckedEventSink;
import org.dwcj.component.event.sink.FocusEventSink;
import org.dwcj.component.event.sink.MouseEnterEventSink;
import org.dwcj.component.event.sink.MouseExitEventSink;
import org.dwcj.component.event.sink.RightMouseDownEventSink;
import org.dwcj.component.event.sink.UncheckedEventSink;
import org.dwcj.component.window.AbstractWindow;
import org.dwcj.util.BBjFunctionalityHelper;

/** A checkbox object. */
public final class CheckBox extends AbstractDwcComponent
    implements HasReadOnly, Focusable, TabTraversable, TextAlignable, HasEnable {

  /**
   * Expanse options for the checkbox.
   */
  public enum Expanse {
    LARGE, MEDIUM, SMALL, XLARGE, XSMALL
  }

  /**
   * Enum that replaced the BBj constants required by the underlying BBj methods to more
   * legible and developer-friendly options.
   */
  public enum HorizontalTextPosition {
    RIGHT(4), LEFT(2), CENTER(0), LEADING(10), TRAILING(11);

    public final Integer position;

    private HorizontalTextPosition(Integer position) {
      this.position = position;
    }
  }

  private EventDispatcher dispatcher = new EventDispatcher();
  private MouseEnterEventSink mouseEnterEventSink;
  private MouseExitEventSink mouseExitEventSink;
  private RightMouseDownEventSink rightMouseDownEventSink;
  private CheckedEventSink checkedEventSink;
  private UncheckedEventSink uncheckedEventSink;
  private FocusEventSink focusEventSink;
  private BlurEventSink blurEventSink;
  private HorizontalTextPosition horizontalTextPosition = HorizontalTextPosition.RIGHT;
  private Boolean checked = false;
  private Boolean indeterminate = false;
  private String name;
  private Boolean required = false;


  /**
   * Constructor initializes the inherited interface member variables to their defaults.
   */
  public CheckBox() {
    this.readOnly = false;
    this.focusable = true;
    this.tabTraversable = true;
    this.textAlignment = Alignment.LEFT;

  }

  /**
   * {@inheritDoc}}
   */
  @Override
  protected void create(AbstractWindow p) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(p);
      byte[] flags =
          BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
      ctrl = w.addCheckBox("", flags);
      this.mouseEnterEventSink = new MouseEnterEventSink(this, dispatcher);
      this.mouseExitEventSink = new MouseExitEventSink(this, dispatcher);
      this.rightMouseDownEventSink = new RightMouseDownEventSink(this, dispatcher);
      this.checkedEventSink = new CheckedEventSink(this, dispatcher);
      this.uncheckedEventSink = new UncheckedEventSink(this, dispatcher);
      this.focusEventSink = new FocusEventSink(this, dispatcher);
      this.blurEventSink = new BlurEventSink(this, dispatcher);
      this.catchUp();
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  /**
   * Adds a checked event for the checkbox component.
   *
   * @param listener the event listener to be added
   * @return The checkbox itself
   */
  public CheckBox addCheckedListener(EventListener<CheckedEvent> listener) {
    if (this.ctrl != null && this.dispatcher.getListenersCount(CheckedEvent.class) == 0) {
      this.checkedEventSink.setCallback();
    }
    dispatcher.addEventListener(CheckedEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addCheckedListener method.
   *
   * @see Checkbox#addCheckedLitener(EventListener)
   * @param listener the event listener to be added
   * @return The checkbox itself
   */
  public CheckBox onChecked(EventListener<CheckedEvent> listener) {
    return addCheckedListener(listener);
  }

  /**
   * Removes a checked event from the checkbox component.
   *
   * @param listener the event listener to be removed
   * @return The checkbox itself
   */
  public CheckBox removeCheckedListener(EventListener<CheckedEvent> listener) {
    dispatcher.removeEventListener(CheckedEvent.class, listener);
    if (this.ctrl != null && this.dispatcher.getListenersCount(CheckedEvent.class) == 0) {
      this.checkedEventSink.removeCallback();
    }
    return this;
  }

  /**
   * Adds an unchecked event for the checkbox component.
   *
   * @param listener the event listener to be added
   * @return The checkbox itself
   */
  public CheckBox addUncheckedListener(EventListener<UncheckedEvent> listener) {
    if (this.ctrl != null && this.dispatcher.getListenersCount(UncheckedEvent.class) == 0) {
      this.uncheckedEventSink.setCallback();
    }
    dispatcher.addEventListener(UncheckedEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addUncheckedListener method.
   *
   * @see Checkbox#addUncheckedListener(EventListener)
   * @param listener the event listener to be added
   * @return The checkbox itself
   */
  public CheckBox onUnchecked(EventListener<UncheckedEvent> listener) {
    return addUncheckedListener(listener);
  }

  /**
   * Removes an unchecked event from the checkbox component.
   *
   * @param listener the event listener to be removed
   * @return The checkbox itself
   */
  public CheckBox removeUncheckedListener(EventListener<UncheckedEvent> listener) {
    dispatcher.removeEventListener(UncheckedEvent.class, listener);
    if (this.ctrl != null && this.dispatcher.getListenersCount(UncheckedEvent.class) == 0) {
      this.uncheckedEventSink.removeCallback();
    }
    return this;
  }

  /**
   * Adds a focus event for the checkbox component.
   *
   * @param listener the event listener to be added
   * @return The checkbox itself
   */
  public CheckBox addFocusListener(EventListener<FocusEvent> listener) {
    if (this.ctrl != null && this.dispatcher.getListenersCount(FocusEvent.class) == 0) {
      this.focusEventSink.setCallback();
    }
    dispatcher.addEventListener(FocusEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addFocusListener method.
   *
   * @see Checkbox#addFocusListener(EventListener)
   * @param listener the event listener to be added
   * @return The checkbox itself
   */
  public CheckBox onFocus(EventListener<FocusEvent> listener) {
    return addFocusListener(listener);
  }

  /**
   * Removes a focus event from the checkbox component.
   *
   * @param listener the event listener to be removed
   * @return The checkbox itself
   */
  public CheckBox removeFocusListener(EventListener<FocusEvent> listener) {
    dispatcher.removeEventListener(FocusEvent.class, listener);
    if (this.ctrl != null && this.dispatcher.getListenersCount(FocusEvent.class) == 0) {
      this.focusEventSink.removeCallback();
    }
    return this;
  }

  /**
   * Adds a blur event for the checkbox component.
   *
   * @param listener the event listener to be added
   * @return The checkbox itself
   */
  public CheckBox addBlurListener(EventListener<BlurEvent> listener) {
    if (this.ctrl != null && this.dispatcher.getListenersCount(BlurEvent.class) == 0) {
      this.blurEventSink.setCallback();
    }
    dispatcher.addEventListener(BlurEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addBlurListener method.
   *
   * @see Checkbox#addBlurListener(EventListener)
   * @param listener the event listener to be added
   * @return The checkbox itself
   */
  public CheckBox onBlur(EventListener<BlurEvent> listener) {
    return addBlurListener(listener);
  }

  /**
   * Removes a blur event from the checkbox component.
   *
   * @param listener the event listener to be removed
   * @return The checkbox itself
   */
  public CheckBox removeBlurListener(EventListener<BlurEvent> listener) {
    dispatcher.removeEventListener(BlurEvent.class, listener);
    if (this.ctrl != null && this.dispatcher.getListenersCount(BlurEvent.class) == 0) {
      this.blurEventSink.removeCallback();
    }
    return this;
  }

  /**
   * Adds a MouseEnter event for the checkbox component.
   *
   * @param listener the event listener to be added
   * @return The checkbox itself
   */
  public CheckBox addMouseEnterListener(EventListener<MouseEnterEvent> listener) {
    if (this.ctrl != null && this.dispatcher.getListenersCount(MouseEnterEvent.class) == 0) {
      this.mouseEnterEventSink.setCallback();
    }
    dispatcher.addEventListener(MouseEnterEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addMouseEnterListener method.
   *
   * @see Checkbox#addMouseEnterListener(EventListener)
   * @param listener the event listener to be added
   * @return The checkbox itself
   */
  public CheckBox onMouseEnter(EventListener<MouseEnterEvent> listener) {
    return addMouseEnterListener(listener);
  }

  /**
   * Removes a MouseEnter event from the checkbox component.
   *
   * @param listener the event listener to be removed
   * @return The checkbox itself
   */
  public CheckBox removeMouseEnterListener(EventListener<MouseEnterEvent> listener) {
    dispatcher.removeEventListener(MouseEnterEvent.class, listener);
    if (this.ctrl != null && this.dispatcher.getListenersCount(MouseEnterEvent.class) == 0) {
      this.mouseEnterEventSink.removeCallback();
    }
    return this;
  }

  /**
   * Adds a MouseExit event for the checkbox component.
   *
   * @param listener the event listener to be added
   * @return The checkbox itself
   */
  public CheckBox addMouseExitListener(EventListener<MouseExitEvent> listener) {
    if (this.ctrl != null && this.dispatcher.getListenersCount(MouseExitEvent.class) == 0) {
      this.mouseExitEventSink.setCallback();
    }
    dispatcher.addEventListener(MouseExitEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addMouseExitListener method.
   *
   * @see Checkbox#addMouseExitListener(EventListener)
   * @param listener the event listener to be added
   * @return The checkbox itself
   */
  public CheckBox onMouseExit(EventListener<MouseExitEvent> listener) {
    return addMouseExitListener(listener);
  }

  /**
   * Removes a MouseExit event from the checkbox component.
   *
   * @param listener the event listener to be removed
   * @return The checkbox itself
   */
  public CheckBox removeMouseExitListener(EventListener<MouseExitEvent> listener) {
    dispatcher.removeEventListener(MouseExitEvent.class, listener);
    if (this.ctrl != null && this.dispatcher.getListenersCount(MouseExitEvent.class) == 0) {
      this.mouseExitEventSink.removeCallback();
    }
    return this;
  }

  /**
   * Adds a MouseExit event for the checkbox component.
   *
   * @param listener the event listener to be added
   * @return The checkbox itself
   */
  public CheckBox addRightMouseDownListener(EventListener<RightMouseDownEvent> listener) {
    if (this.ctrl != null && this.dispatcher.getListenersCount(RightMouseDownEvent.class) == 0) {
      this.rightMouseDownEventSink.setCallback();
    }
    dispatcher.addEventListener(RightMouseDownEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addRightMouseDownListener method.
   *
   * @see Checkbox#addRightMouseDownListener(EventListener)
   * @param listener the event listener to be added
   * @return The checkbox itself
   */
  public CheckBox onRightMouseDown(EventListener<RightMouseDownEvent> listener) {
    return addRightMouseDownListener(listener);
  }

  /**
   * Removes a RightMouseDown event from the checkbox component.
   *
   * @param listener the event listener to be removed
   * @return The checkbox itself
   */
  public CheckBox removeRightMouseDownListener(EventListener<RightMouseDownEvent> listener) {
    dispatcher.removeEventListener(RightMouseDownEvent.class, listener);
    if (this.ctrl != null && this.dispatcher.getListenersCount(RightMouseDownEvent.class) == 0) {
      this.rightMouseDownEventSink.removeCallback();
    }
    return this;
  }

  /**
   * The default horizontal text position is RIGHT.
   *
   * @return This method returns the horizontal position of the text in the CheckBox control.
   */
  public HorizontalTextPosition getHorizontalTextPosition() {
    if (this.ctrl != null) {
      return this.horizontalTextPosition;
    }
    return HorizontalTextPosition.RIGHT;
  }

  /**
   * Sets the horizontal text position.
   *
   * @param position the position where the text should be placed.
   * @see HorizontalTextPosition
   *
   * @return this object.
   */
  public CheckBox setHorizontalTextPosition(HorizontalTextPosition position) {
    if (this.ctrl != null) {
      try {
        ((BBjCheckBox) this.ctrl).setHorizontalTextPosition(position.position);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.horizontalTextPosition = position;
    return this;
  }


  /**
   * Returns whether the BBjCheckBox is checked on or off (false = not checked, true = checked).
   *
   * @return false if not checked, true if checked.
   */
  public Boolean isChecked() {
    if (this.ctrl != null) {
      try {
        return ((BBjCheckBox) this.ctrl).isSelected();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return checked;
  }

  /**
   * Sets the box to be either checked or unchecked.
   *
   * @param checked Wether the box should be checked or not.
   *
   * @return this object.
   */
  public CheckBox setChecked(Boolean checked) {
    if (this.ctrl != null) {
      try {
        ((BBjCheckBox) this.ctrl).setSelected(checked);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.checked = checked;
    return this;
  }

  @Override
  public CheckBox setText(String text) {
    super.setText(text);
    return this;
  }

  @Override
  public CheckBox setVisible(Boolean visible) {
    super.setVisible(visible);
    return this;
  }

  @Override
  public CheckBox setEnabled(boolean enabled) {
    super.setComponentEnabled(enabled);
    return this;
  }

  @Override 
  public boolean isEnabled(){
    return super.isComponentEnabled();
  }

  @Override
  public CheckBox setTooltipText(String text) {
    super.setTooltipText(text);
    return this;
  }

  @Override
  public CheckBox setAttribute(String attribute, String value) {
    super.setAttribute(attribute, value);
    return this;
  }

  @Override
  public CheckBox setStyle(String property, String value) {
    super.setStyle(property, value);
    return this;
  }

  @Override
  public CheckBox addClassName(String selector) {
    super.addClassName(selector);
    return this;
  }

  @Override
  public CheckBox removeClassName(String selector) {
    super.removeClassName(selector);
    return this;
  }

  /**
   * Sets the expanse for the checkbox.
   *
   * @param expanse The desired Expanse enum
   * @return The object itself
   */
  public CheckBox setExpanse(Expanse expanse) {
    super.setControlExpanse(expanse);
    return this;
  }

  /**
   * Returns whether the BBjCheckBox is editable (false = not editable, true = editable).
   *
   * @return false if not editable, true if editable.
   */
  @Override
  public Boolean isReadOnly() {
    if (this.ctrl != null) {
      try {
        return !((BBjCheckBox) ctrl).isEditable();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.readOnly;
  }

  /**
   * this method sets whether the CheckBox can be edited. True is editable, false is uneditable.
   *
   * @param editable if true the control is editable
   * @return this
   */
  @Override
  public CheckBox setReadOnly(Boolean editable) {
    if (this.ctrl != null) {
      try {
        ((BBjCheckBox) this.ctrl).setEditable(!editable);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.readOnly = editable;
    return this;
  }

  @Override
  public Boolean isFocusable() {
    if (this.ctrl != null) {
      try {
        return ((BBjCheckBox) ctrl).isFocusable();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.focusable;
  }

  @Override
  public CheckBox setFocusable(Boolean focusable) {
    if (this.ctrl != null) {
      try {
        ((BBjCheckBox) this.ctrl).setFocusable(focusable);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.focusable = focusable;
    return this;
  }

  @Override
  public Boolean isTabTraversable() {
    if (this.ctrl != null) {
      try {
        return ((BBjCheckBox) ctrl).isTabTraversable();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.tabTraversable;
  }

  @Override
  public CheckBox setTabTraversable(Boolean traversable) {
    if (this.ctrl != null) {
      try {
        ((BBjCheckBox) this.ctrl).setTabTraversable(traversable);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.tabTraversable = traversable;
    return this;
  }

  @Override
  public Alignment getTextAlignment() {
    return this.textAlignment;
  }

  @Override
  public CheckBox setTextAlignment(Alignment alignment) {
    if (this.ctrl != null) {
      try {
        ((BBjCheckBox) this.ctrl).setAlignment(alignment.textPosition);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.textAlignment = alignment;
    return this;
  }

  /**
   * Returns wether or not the checkbox is focused.
   *
   * @return A Boolean representing wether or not the checkbox is focused.
   */
  public boolean hasFocus() {
    return Boolean.parseBoolean(super.getAttribute("has-focus"));
  }

  /**
   * The method to set wether the checkbox is indeterminate or not.
   *
   * @param value a boolean representing wether the checkbox is indeterminate.
   *
   * @return this.
   */
  public CheckBox setIndeterminate(Boolean value) {
    if (ctrl != null) {
      try {
        ctrl.setAttribute("indeterminate", value.toString());
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.indeterminate = value;
    return this;
  }

  /**
   * Returns wether or not the checkbox is indeterminate.
   *
   * @return A Boolean representing wether or not the checkbox is indeterminate.
   */
  public Boolean getIndeterminate() {
    return this.indeterminate;
  }

  /**
   * The method to set the label of the checkbox.
   *
   * @param value the label text to be set.
   *
   * @return this.
   */
  public CheckBox setLabel(String value) {
    super.setProperty("label", value);
    return this;
  }

  /**
   * Returns the label of the control.
   *
   * @return A String representing the label.
   */
  public String getLabel() {
    return (String) super.getProperty("label");
  }

  /**
   * The method to set the name of the checkbox.
   *
   * @param value the name text to be set.
   *
   * @return this.
   */
  public CheckBox setName(String value) {
    if (ctrl != null) {
      try {
        ctrl.setAttribute("name", value);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.name = value;
    return this;
  }

  /**
   * Returns the name of the control.
   *
   * @return A String representing the name.
   */
  public String getName() {
    return this.name;
  }

  /**
   * The method to enable or disable the checkbox being required.
   *
   * @param value the boolean to determine disable/enable.
   *
   * @return this.
   */
  public CheckBox setRequired(Boolean value) {
    if (ctrl != null) {
      try {
        ctrl.setAttribute("required", value.toString());
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.required = value;
    return this;
  }

  /**
   * Returns wether or not the checkbox is required.
   *
   * @return A Boolean representing wether or not the checkbox is required.
   */
  public Boolean getRequired() {
    return this.required;
  }

  @Override
  @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now
  protected void catchUp() throws IllegalAccessException {
    if (Boolean.TRUE.equals(this.getCaughtUp())) {
      throw new IllegalAccessException("catchUp cannot be called twice");
    }
    super.catchUp();

    if (this.checked != null) {
      this.setChecked(this.checked);
    }


    if (this.horizontalTextPosition != HorizontalTextPosition.RIGHT) {
      try {
        ((BBjCheckBox) ctrl).setHorizontalTextPosition(horizontalTextPosition.position);
      } catch (BBjException e) {
        Environment.logError(e);
      }
      this.setHorizontalTextPosition(this.horizontalTextPosition);
    }

    if (Boolean.TRUE.equals(this.readOnly)) {
      this.setReadOnly(true);
    }

    if (Boolean.FALSE.equals(this.focusable)) {
      this.setFocusable(this.focusable);
    }

    if (Boolean.FALSE.equals(this.tabTraversable)) {
      this.setTabTraversable(this.tabTraversable);
    }

    if (this.textAlignment != Alignment.LEFT) {
      this.setTextAlignment(this.textAlignment);
    }

    if (Boolean.TRUE.equals(this.indeterminate)) {
      this.setIndeterminate(this.indeterminate);
    }

    if (this.name != null) {
      this.setName(this.name);
    }

    if (Boolean.TRUE.equals(this.required)) {
      this.setRequired(this.required);
    }
  }

}
