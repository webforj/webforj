package org.dwcj.component.radiobutton;

import com.basis.bbj.proxies.sysgui.BBjRadioButton;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.Environment;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.Focusable;
import org.dwcj.component.HasReadOnly;
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


/**
 * The class itself extending the abstract DWC Component and implementing interfaces.
 */
public final class RadioButton extends AbstractDwcComponent
    implements HasReadOnly, Focusable, TextAlignable {

  private BBjRadioButton bbjRadioButton;
  private EventDispatcher dispatcher = new EventDispatcher();
  private MouseEnterEventSink mouseEnterEventSink;
  private MouseExitEventSink mouseExitEventSink;
  private RightMouseDownEventSink rightMouseDownEventSink;
  private CheckedEventSink checkedEventSink;
  private UncheckedEventSink uncheckedEventSink;
  private FocusEventSink focusEventSink;
  private BlurEventSink blurEventSink;


  @Override
  public Alignment getTextAlignment() {
    return this.textAlignment;
  }

  @Override
  public RadioButton setTextAlignment(Alignment alignment) {
    if (this.ctrl != null) {
      try {
        bbjRadioButton.setAlignment(alignment.textPosition);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.textAlignment = alignment;
    return this;
  }

  /**
   * Enum values with respective values for the expanse of the text.
   */
  public enum Expanse {
    LARGE, MEDIUM, SMALL, XLARGE, XSMALL
  }

  /**
   * Enum values with respective values for the activation of radioButton.
   */
  public enum Activation {
    AUTO, MANUAL
  }

  /**
   * Enum values with respective values for the horizontal text position.
   */
  public enum HorizontalTextPosition {
    RIGHT(4), LEFT(2), CENTER(0), LEADING(10), TRAILING(11);

    public final Integer position;

    private HorizontalTextPosition(Integer position) {
      this.position = position;
    }
  }

  private Boolean checked = false;
  private Boolean disabled = false;
  private Boolean switched = false;
  private Boolean required = false;
  private Boolean invalid = false;
  private String invalidMessage = "";
  private String label = "";
  private Activation activation = Activation.MANUAL;
  private Expanse expanse = Expanse.LARGE;
  private HorizontalTextPosition horizontalTextPosition = HorizontalTextPosition.RIGHT;

  /**
   * The Constructor with expected behavior after an instance of it created.
   */
  public RadioButton() {
    this.readOnly = false;
    this.focusable = true;
    this.tabTraversable = true;
    this.textAlignment = Alignment.MIDDLE;
  }

  @Override
  protected void create(AbstractWindow p) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(p);
      byte[] flags =
        BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
      ctrl = w.addRadioButton("", flags);
      this.mouseEnterEventSink = new MouseEnterEventSink(this, dispatcher);
      this.mouseExitEventSink = new MouseExitEventSink(this, dispatcher);
      this.rightMouseDownEventSink = new RightMouseDownEventSink(this, dispatcher);
      this.checkedEventSink = new CheckedEventSink(this, dispatcher);
      this.uncheckedEventSink = new UncheckedEventSink(this, dispatcher);
      this.focusEventSink = new FocusEventSink(this, dispatcher);
      this.blurEventSink = new BlurEventSink(this, dispatcher);
      bbjRadioButton = (BBjRadioButton) ctrl;
      catchUp();
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  /**
   * Adds a checked event for the radiobutton component.
   *
   * @param listener the event listener to be added
   * @return The radiobutton itself
   */
  public RadioButton addCheckedListener(EventListener<CheckedEvent> listener) {
    if (this.ctrl != null && this.dispatcher.getListenersCount(CheckedEvent.class) == 0) {
      this.checkedEventSink.setCallback();
    }
    dispatcher.addEventListener(CheckedEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addCheckedListener method.
   *
   * @see RadioButton#addCheckedLitener(EventListener)
   * @param listener the event listener to be added
   * @return The radiobutton itself
   */
  public RadioButton onChecked(EventListener<CheckedEvent> listener) {
    return addCheckedListener(listener);
  }

  /**
   * Removes a checked event from the radiobutton component.
   *
   * @param listener the event listener to be removed
   * @return The radiobutton itself
   */
  public RadioButton removeCheckedListener(EventListener<CheckedEvent> listener) {
    dispatcher.removeEventListener(CheckedEvent.class, listener);
    if (this.ctrl != null && this.dispatcher.getListenersCount(CheckedEvent.class) == 0) {
      this.checkedEventSink.removeCallback();
    }
    return this;
  }

  /**
   * Adds an unchecked event for the RadioButton component.
   *
   * @param listener the event listener to be added
   * @return The RadioButton itself
   */
  public RadioButton addUncheckedListener(EventListener<UncheckedEvent> listener) {
    if (this.ctrl != null && this.dispatcher.getListenersCount(UncheckedEvent.class) == 0) {
      this.uncheckedEventSink.setCallback();
    }
    dispatcher.addEventListener(UncheckedEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addUncheckedListener method.
   *
   * @see RadioButton#addUncheckedListener(EventListener)
   * @param listener the event listener to be added
   * @return The RadioButton itself
   */
  public RadioButton onUnchecked(EventListener<UncheckedEvent> listener) {
    return addUncheckedListener(listener);
  }

  /**
   * Removes an unchecked event from the RadioButton component.
   *
   * @param listener the event listener to be removed
   * @return The RadioButton itself
   */
  public RadioButton removeUncheckedListener(EventListener<UncheckedEvent> listener) {
    dispatcher.removeEventListener(UncheckedEvent.class, listener);
    if (this.ctrl != null && this.dispatcher.getListenersCount(UncheckedEvent.class) == 0) {
      this.uncheckedEventSink.removeCallback();
    }
    return this;
  }

  /**
   * Adds a focus event for the RadioButton component.
   *
   * @param listener the event listener to be added
   * @return The RadioButton itself
   */
  public RadioButton addFocusListener(EventListener<FocusEvent> listener) {
    if (this.ctrl != null && this.dispatcher.getListenersCount(FocusEvent.class) == 0) {
      this.focusEventSink.setCallback();
    }
    dispatcher.addEventListener(FocusEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addFocusListener method.
   *
   * @see RadioButton#addFocusListener(EventListener)
   * @param listener the event listener to be added
   * @return The RadioButton itself
   */
  public RadioButton onFocus(EventListener<FocusEvent> listener) {
    return addFocusListener(listener);
  }

  /**
   * Removes a focus event from the RadioButton component.
   *
   * @param listener the event listener to be removed
   * @return The RadioButton itself
   */
  public RadioButton removeFocusListener(EventListener<FocusEvent> listener) {
    dispatcher.removeEventListener(FocusEvent.class, listener);
    if (this.ctrl != null && this.dispatcher.getListenersCount(FocusEvent.class) == 0) {
      this.focusEventSink.removeCallback();
    }
    return this;
  }

  /**
   * Adds a blur event for the RadioButton component.
   *
   * @param listener the event listener to be added
   * @return The RadioButton itself
   */
  public RadioButton addBlurListener(EventListener<BlurEvent> listener) {
    if (this.ctrl != null && this.dispatcher.getListenersCount(BlurEvent.class) == 0) {
      this.blurEventSink.setCallback();
    }
    dispatcher.addEventListener(BlurEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addBlurListener method.
   *
   * @see RadioButton#addBlurListener(EventListener)
   * @param listener the event listener to be added
   * @return The RadioButton itself
   */
  public RadioButton onBlur(EventListener<BlurEvent> listener) {
    return addBlurListener(listener);
  }

  /**
   * Removes a blur event from the RadioButton component.
   *
   * @param listener the event listener to be removed
   * @return The RadioButton itself
   */
  public RadioButton removeBlurListener(EventListener<BlurEvent> listener) {
    dispatcher.removeEventListener(BlurEvent.class, listener);
    if (this.ctrl != null && this.dispatcher.getListenersCount(BlurEvent.class) == 0) {
      this.blurEventSink.removeCallback();
    }
    return this;
  }

  /**
   * Adds a MouseEnter event for the RadioButton component.
   *
   * @param listener the event listener to be added
   * @return The RadioButton itself
   */
  public RadioButton addMouseEnterListener(EventListener<MouseEnterEvent> listener) {
    if (this.ctrl != null && this.dispatcher.getListenersCount(MouseEnterEvent.class) == 0) {
      this.mouseEnterEventSink.setCallback();
    }
    dispatcher.addEventListener(MouseEnterEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addMouseEnterListener method.
   *
   * @see RadioButton#addMouseEnterListener(EventListener)
   * @param listener the event listener to be added
   * @return The RadioButton itself
   */
  public RadioButton onMouseEnter(EventListener<MouseEnterEvent> listener) {
    return addMouseEnterListener(listener);
  }

  /**
   * Removes a MouseEnter event from the RadioButton component.
   *
   * @param listener the event listener to be removed
   * @return The RadioButton itself
   */
  public RadioButton removeMouseEnterListener(EventListener<MouseEnterEvent> listener) {
    dispatcher.removeEventListener(MouseEnterEvent.class, listener);
    if (this.ctrl != null && this.dispatcher.getListenersCount(MouseEnterEvent.class) == 0) {
      this.mouseEnterEventSink.removeCallback();
    }
    return this;
  }

  /**
   * Adds a MouseExit event for the RadioButton component.
   *
   * @param listener the event listener to be added
   * @return The RadioButton itself
   */
  public RadioButton  addMouseExitListener(EventListener<MouseExitEvent> listener) {
    if (this.ctrl != null && this.dispatcher.getListenersCount(MouseExitEvent.class) == 0) {
      this.mouseExitEventSink.setCallback();
    }
    dispatcher.addEventListener(MouseExitEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addMouseExitListener method.
   *
   * @see RadioButton #addMouseExitListener(EventListener)
   * @param listener the event listener to be added
   * @return The RadioButton  itself
   */
  public RadioButton  onMouseExit(EventListener<MouseExitEvent> listener) {
    return addMouseExitListener(listener);
  }

  /**
   * Removes a MouseExit event from the RadioButton  component.
   *
   * @param listener the event listener to be removed
   * @return The RadioButton  itself
   */
  public RadioButton  removeMouseExitListener(EventListener<MouseExitEvent> listener) {
    dispatcher.removeEventListener(MouseExitEvent.class, listener);
    if (this.ctrl != null && this.dispatcher.getListenersCount(MouseExitEvent.class) == 0) {
      this.mouseExitEventSink.removeCallback();
    }
    return this;
  }

  /**
   * Adds a MouseExit event for the RadioButton  component.
   *
   * @param listener the event listener to be added
   * @return The RadioButton itself
   */
  public RadioButton  addRightMouseDownListener(EventListener<RightMouseDownEvent> listener) {
    if (this.ctrl != null && this.dispatcher.getListenersCount(RightMouseDownEvent.class) == 0) {
      this.rightMouseDownEventSink.setCallback();
    }
    dispatcher.addEventListener(RightMouseDownEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addRightMouseDownListener method.
   *
   * @see RadioButton #addRightMouseDownListener(EventListener)
   * @param listener the event listener to be added
   * @return The RadioButton  itself
   */
  public RadioButton onRightMouseDown(EventListener<RightMouseDownEvent> listener) {
    return addRightMouseDownListener(listener);
  }

  /**
   * Removes a RightMouseDown event from the RadioButton  component.
   *
   * @param listener the event listener to be removed
   * @return The RadioButton  itself
   */
  public RadioButton  removeRightMouseDownListener(EventListener<RightMouseDownEvent> listener) {
    dispatcher.removeEventListener(RightMouseDownEvent.class, listener);
    if (this.ctrl != null && this.dispatcher.getListenersCount(RightMouseDownEvent.class) == 0) {
      this.rightMouseDownEventSink.removeCallback();
    }
    return this;
  }

  /**
   * Checks from Clientside if the component has focus.
   *
   * @return checks if component has focus.
   */
  public Boolean hasFocus() {
    return Boolean.valueOf(super.getAttribute("hasFocus"));
  }

  /**
   * True to disable the radio button , false to enable it.
   *
   * @return checks if component is disabled.
   */
  public Boolean isDisabled() {
    if (this.ctrl != null) {
      try {
        return !bbjRadioButton.isEnabled();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.disabled;
  }

  /**
   * Gets the position of the text alignment in relation to the radio button.
   *
   * @return Enum value of the horizontal text position for the component.
   */
  public HorizontalTextPosition getHorizontalTextPosition() {
    return this.horizontalTextPosition;
  }

  /**
   * Sets the position of the text horizontally.
   *
   * @return The component itself.
   */
  public RadioButton setHorizontalTextPosition(HorizontalTextPosition position) {
    if (this.ctrl != null) {
      try {
        bbjRadioButton.setHorizontalTextPosition(position.position);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.horizontalTextPosition = position;
    return this;
  }


  /**
   * Returns true to indicate that the user input is invalid, false otherwise.
   *
   * @return Boolean value representing if the users input is valid.
   */
  public Boolean isInvalid() {
    if (this.ctrl != null) {
      try {
        return Boolean.valueOf(bbjRadioButton.getAttribute("invalid"));
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.invalid;
  }

  /**
   * Indicates whether the users input is valid or not.
   *
   * @param invalid Boolean to check the validity of users input.
   * @return The component itself.
   */
  public RadioButton setInvalid(boolean invalid) {
    if (this.ctrl != null) {
      try {
        bbjRadioButton.setAttribute("invalid", String.valueOf(invalid));
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.invalid = invalid;
    return this;
  }

  /**
   * An error message to present to the user when the component is invalid.
   *
   * @return The Invalid message for the user.
   */
  public String getInvalidMessage() {
    return this.invalidMessage;
  }

  /**
   * Sets the message to present to the user when component is invalid.
   *
   * @param message A string message to present to the user when component is invalid.
   * @return The component itself.
   */
  public RadioButton setInvalidMessage(String message) {
    if (this.ctrl != null) {
      try {
        bbjRadioButton.setAttribute("invalidMessage", message);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.invalidMessage = message;
    return this;
  }

  /**
   * Returns true if the radio button is selected, false otherwise.
   *
   * @return Boolean value representing the radio button's selection value
   */
  public Boolean isChecked() {
    if (this.ctrl != null) {
      try {
        return bbjRadioButton.isSelected();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.checked;
  }

  /**
   * Sets the selection value of the radio button - true if the button should be selected, false if
   * not.
   *
   * @param checked Boolean for desired selection state of the button.
   * @return The component itself.
   */
  public RadioButton setChecked(boolean checked) {
    if (this.ctrl != null) {
      try {
        bbjRadioButton.setSelected(checked);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.checked = checked;
    return this;
  }

  /**
   * Returns true if the radio button is submittable, false otherwise.
   *
   * @return Checks if the radioButton is submittable.
   */
  public Boolean isRequired() {
    if (this.ctrl != null) {
      try {
        return Boolean.valueOf(bbjRadioButton.getAttribute("required"));
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.required;
  }

  /**
   * Returns Radio button label.
   *
   * @return The radio button label.
   */
  public String getLabel() {
    return this.label;
  }

  /**
   * The radio button label.
   *
   * @param label The radio button label.
   * @return The component itself.
   */
  public RadioButton setLabel(String label) {
    if (this.ctrl != null) {
      try {
        bbjRadioButton.setAttribute("label", label);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.label = label;
    return this;
  }

  /**
   * A value is required or must be check for the form to be submittable.
   *
   * @param required Boolean for the form to be submittable.
   * @return The component itself.
   */
  public RadioButton setRequired(boolean required) {
    if (this.ctrl != null) {
      try {
        bbjRadioButton.setAttribute("type", "required");
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.required = required;
    return this;
  }


  /**
   * Returns true if the radioButton is rendered, false otherwise.
   *
   * @return Checks if will be rendered.
   */
  public Boolean isSwitched() {
    if (this.ctrl != null) {
      try {
        return Boolean.valueOf(bbjRadioButton.getAttribute("switch"));
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.switched;
  }

  /**
   * When true, the radio button will be rendered as a switch.
   *
   * @param switched Boolean to be rendered as a switch.
   * @return The component itself.
   */
  public RadioButton setSwitched(boolean switched) {
    if (this.ctrl != null) {
      try {
        bbjRadioButton.setAttribute("type", "switch");
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.switched = switched;
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Boolean isReadOnly() {
    if (this.ctrl != null) {
      try {
        return !bbjRadioButton.isEditable();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.readOnly;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public RadioButton setReadOnly(Boolean readOnly) {
    if (this.ctrl != null) {
      try {
        bbjRadioButton.setEditable(!readOnly);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.readOnly = readOnly;
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Boolean isFocusable() {
    if (this.ctrl != null) {
      try {
        bbjRadioButton.isFocusable();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.focusable;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public RadioButton setFocusable(Boolean focusable) {
    if (this.ctrl != null) {
      try {
        bbjRadioButton.setFocusable(focusable);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.focusable = focusable;
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public RadioButton setText(String text) {
    super.setText(text);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public RadioButton setVisible(Boolean visible) {
    super.setVisible(visible);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public RadioButton setEnabled(Boolean enabled) {
    super.setEnabled(enabled);
    this.disabled = !enabled;
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public RadioButton setTooltipText(String text) {
    super.setTooltipText(text);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public RadioButton setAttribute(String attribute, String value) {
    super.setAttribute(attribute, value);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public RadioButton setUserData(String key, Object data) {
    super.setUserData(key, data);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public RadioButton setStyle(String property, String value) {
    super.setStyle(property, value);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public RadioButton removeAttribute(String attribute) {
    super.removeAttribute(attribute);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public RadioButton removeStyle(String property) {
    super.removeStyle(property);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public RadioButton addClassName(String selector) {
    super.addClassName(selector);
    return this;
  }


  /**
   * {@inheritDoc}
   */
  @Override
  public RadioButton removeClassName(String selector) {
    super.removeClassName(selector);
    return this;
  }

  /**
   * Sets the activation of the ratio button from the enum with component-specific applicable
   * activation values.
   *
   * <pre>
   * {@code}
   * RadioButton button = new RadioButton.setActivation(RadioButton.Activation.MANUAL);
   * }
   * </pre>
   * * @param activation The enum value representing the desired activation. * @return the class
   * itself.
   */
  public RadioButton setActivation(Activation activation) {
    super.setAttribute("activation", String.valueOf(activation).toLowerCase());
    this.activation = activation;
    return this;
  }

  /**
   * Returns Activation of RadioButton.
   *
   * @return The Activation which was set from the setActivation() method.
   */
  public Activation getActivation() {
    return this.activation;
  }


  /**
   * Sets the expanse of the radio button from the enum with component-specific applicable expanse
   * values.
   *
   * <pre>
   * {@code
   * RadioButton button = new RadioButton().setExpanse(RadioButton.Expanse.LARGE);
   * }
   * </pre>
   *
   * @param expanse The enum value representing the desired expanse
   * @return the class.
   */
  public RadioButton setExpanse(Expanse expanse) {
    this.expanse = expanse;
    return this;
  }

  /**
   * Returns Expanse of RadioButton.
   *
   * @return The Expanse which was set from the setExpanse() method.
   */
  public Expanse getExpanse() {
    return this.expanse;
  }

  @Override
  @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list
  // of checks
  protected void catchUp() throws IllegalAccessException {
    if (Boolean.TRUE.equals(this.getCaughtUp())) {
      throw new IllegalAccessException("catchUp cannot be called twice");
    }
    super.catchUp();

    if (Boolean.TRUE.equals(this.checked)) {
      this.setChecked(this.checked);
    }

    if (this.textAlignment != Alignment.MIDDLE) {
      this.setTextAlignment(this.textAlignment);
    }

    if (this.horizontalTextPosition != HorizontalTextPosition.RIGHT) {
      try {
        bbjRadioButton.setHorizontalTextPosition(horizontalTextPosition.position);
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

    if (Boolean.FALSE.equals(this.checked)) {
      this.setChecked(this.checked);
    }

    if (Boolean.TRUE.equals(this.switched)) {
      this.setSwitched(this.switched);
    }

    if (Boolean.TRUE.equals(this.disabled)) {
      this.setEnabled(!this.disabled);
    }

    if (Boolean.TRUE.equals(this.required)) {
      this.setRequired(this.required);
    }
  }
}
