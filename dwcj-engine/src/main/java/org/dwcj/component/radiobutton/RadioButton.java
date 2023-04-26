package org.dwcj.component.radiobutton;

import com.basis.bbj.proxies.sysgui.BBjRadioButton;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import java.util.ArrayList;
import java.util.function.Consumer;
import org.dwcj.App;
import org.dwcj.Environment;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.Focusable;
import org.dwcj.component.HasReadOnly;
import org.dwcj.component.TabTraversable;
import org.dwcj.component.TextAlignable;
import org.dwcj.component.radiobutton.event.RadioButtonCheckEvent;
import org.dwcj.component.radiobutton.sink.RadioButtonCheckEventSink;
import org.dwcj.component.window.AbstractWindow;
import org.dwcj.util.BBjFunctionalityHelper;


/**
 * The class itself extending the abstract DWC Component and implementing interfaces.
 */
public final class RadioButton extends AbstractDwcComponent
    implements HasReadOnly, Focusable, TabTraversable, TextAlignable {

  private BBjRadioButton bbjRadioButton;


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

  private ArrayList<Consumer<RadioButtonCheckEvent>> callbacks = new ArrayList<>();
  private RadioButtonCheckEventSink checkEventSink;
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
      ctrl = w.addRadioButton(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1,
          BASISNUMBER_1, BASISNUMBER_1, "", flags);
      bbjRadioButton = (BBjRadioButton) ctrl;
      catchUp();
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  /**
   * register an event callback for a checkOn or checkOff event.
   *
   * @param callback A method to receive the onCheck event
   * @return the control itself
   */
  public RadioButton onChange(Consumer<RadioButtonCheckEvent> callback) {
    if (this.ctrl != null) {
      if (this.checkEventSink == null) {
        this.checkEventSink = new RadioButtonCheckEventSink(this);
      }
      this.checkEventSink.addCallback(callback);
    } else {
      this.callbacks.add(callback);
    }
    return this;
  }

  /**
   * Returns the ID of a button - IDs are assigned when a control is added to a panel, not before.
   *
   * @return The ID of the control which has been added to a panel.
   */
  public Integer getButtonId() {
    if (this.ctrl != null) {
      try {
        return bbjRadioButton.getID();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    App.consoleError(
        "ID cannot be fetched as control does not yet exist. Please add control to a window first");
    return null;
  }

  public Boolean hasFocus() {
    return Boolean.valueOf(super.getAttribute("hasFocus"));
  }

  /**
   * True to disable the radio button , false to enable it.
   *
   * @return checks if control is disabled.
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
   * @return Enum value of the horizontal text position for the control.
   */
  public HorizontalTextPosition getHorizontalTextPosition() {
    return this.horizontalTextPosition;
  }

  /**
   * Sets the position of the text horizontally.
   *
   * @return The control itself.
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
   * @return The control itself.
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
   * An error message to present to the user when the control is invalid.
   *
   * @return The Invalid message for the user.
   */
  public String getInvalidMessage() {
    return this.invalidMessage;
  }

  /**
   * Sets the message to present to the user when control is invalid.
   *
   * @param message A string message to present to the user when control is invalid.
   * @return The control itself.
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
   * @return The control itself.
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
   * @return The control itself.
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
   * @return The control itself.
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
   * @return The control itself.
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
  public RadioButton setId(String elementId) {
    super.setId(elementId);
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
   * Sets the activation of the ratio button from the enum with control-specific applicable
   * activation values.
   *
   * <pre>
   * {@code}
   * RadioButton button = new RadioButton.setActivation(RadioButton.Activation.MANUAL);
   * }
   * </pre>
   *
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
   * Sets the expanse of the radio button from the enum with control-specific applicable expanse
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

    if (!this.callbacks.isEmpty()) {
      this.checkEventSink = new RadioButtonCheckEventSink(this);
      while (!this.callbacks.isEmpty()) {
        this.checkEventSink.addCallback(this.callbacks.remove(0));
      }
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

    if (Boolean.FALSE.equals(this.tabTraversable)) {
      this.setTabTraversable(this.tabTraversable);
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
