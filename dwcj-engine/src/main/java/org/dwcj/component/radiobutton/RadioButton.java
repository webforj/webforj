package org.dwcj.component.radiobutton;

import com.basis.bbj.proxies.sysgui.BBjRadioButton;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.resource.preprocessor;
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
import org.dwcj.component.radiobutton.event.RadioButtonCheckEvent;
import org.dwcj.component.radiobutton.sink.RadioButtonCheckEventSink;
import org.dwcj.component.window.AbstractWindow;
import org.dwcj.util.BBjFunctionalityHelper;


/**
  * The class itself extending the abstract DWC Component and implementing interfaces.
  */
public final class RadioButton extends AbstractDwcComponent
    implements HasReadOnly, Focusable, TabTraversable {

  private BBjRadioButton bbjRadioButton;

  /**
   * Enum values with respective values for the expanse of the text.
   */
  public enum Expanse {
    LARGE, MEDIUM, SMALL, XLARGE, XSMALL
  }

  /**
   * Enum values with respective values for the alignment of text position.
   */
  public enum Alignment {
    CENTER(16384), LEFT(8192), RIGHT(32768);

    public final Integer alignment;

    private Alignment(Integer alignment) {
      this.alignment = alignment;
    }
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
  private Boolean valid = false;
  private Boolean required = false;
  private Boolean autoValidate = true;
  private Boolean autoValidateOnLoad = false;
  private Boolean autoWasValidated = false;
  private HorizontalTextPosition horizontalTextPosition = HorizontalTextPosition.RIGHT;
  private Alignment alignment = Alignment.LEFT;

  /**
   * The Constructor with expected behavior after an instance of it created.
   */
  public RadioButton() {
    this.readOnly = false;
    this.focusable = true;
    this.tabTraversable = true;
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

  /**
   * Gets the aligntment of text in radio button.
   *
   * @return Enum value of the alignment for the control.
   */
  public Alignment getAlignment() {
    return this.alignment;
  }

  /**
   * Sets the alignment of the text.
   *
   * @return The control itself.
   */
  public RadioButton setAlignment(Alignment align) {
    if (this.ctrl != null) {
      try {
        bbjRadioButton.setAlignment(align.alignment);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.alignment = align;
    return this;
  }

  /**
   * True to disable the radio button , false to enable it.

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
   * Returns true if the radio button is validated, false otherwise.
   *
   * @return Checks if automatically validated after validation.
   */
  public Boolean isAutoWasValidated() {
    if (this.ctrl != null) {
      try {
        return Boolean.valueOf(bbjRadioButton.getAttribute("autoWasValidated"));
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.autoWasValidated;
  }

  /**
   * When true , the control will switch the valid property automatically
   *      after the control is validated and became valid.
   *
   * @param autoWasValidate Boolean to switch automatically after
   *        validation.
   * @return The control itself.
   */
  public RadioButton setAutoWasValidate(boolean autoWasValidate) {
    if (this.ctrl != null) {
      try {
        bbjRadioButton.setAttribute("type", "autoWasValidate");
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.autoWasValidated = autoWasValidate;
    return this;
  }

  /**
   * Returns true if the radio button is validated after first load, false otherwise.
   *
   * @return Checks if validated after first load.
   */ 
  public Boolean isAutoValidatedOnLoad() {
    if (this.ctrl != null) {
      try {
        return Boolean.valueOf(bbjRadioButton.getAttribute("autoValidateOnLoad"));
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.autoValidateOnLoad;
  }

  /**
   * When true , then the control will be validated when it is loaded for the first time.
   *
   * @param autoValidateOnLoad Boolean to validate after first load.
   * @return The control itself.
   */
  public RadioButton setAutoValidateOnLoad(boolean autoValidateOnLoad) {
    if (this.ctrl != null) {
      try {
        bbjRadioButton.setAttribute("type", "autoValidateOnLoad");
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.autoValidateOnLoad = autoValidateOnLoad;
    return this;
  }

  /**
   * Returns true if the radio button is validated, false otherwise.
   *
   * @return Checks if validated.
   */ 
  public Boolean isAutoValidated() {
    if (this.ctrl != null) {
      try {
        return Boolean.valueOf(bbjRadioButton.getAttribute("autoValidate"));
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.autoValidate;
  }

  /**
   * When true , then the control will be validated with every change.
   *
   * @param autoValidate Boolean to validate with every change.
   * @return The control itself.
   */
  public RadioButton setAutoValidate(boolean autoValidate) {
    if (this.ctrl != null) {
      try {
        bbjRadioButton.setAttribute("type", "autoValidate");
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.autoValidate = autoValidate;
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
   *  A value is required or must be check for the form to be submittable.
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
   * Returns true if the radio button is valid, false otherwise.
   *
   * @return Checks if valid.
   */ 
  public Boolean isValid() {
    if (this.ctrl != null) {
      try {
        return Boolean.valueOf(bbjRadioButton.getAttribute("valid"));
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.valid;
  }

  /**
   *  A value is required or must be check for the form to be submittable.
   *
   * @param valid Boolean for the form to be submittable.
   * @return The control itself.
   */  
  public RadioButton setValid(boolean valid) {
    if (this.ctrl != null) {
      try {
        bbjRadioButton.setAttribute("type", "valid");
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.valid = valid;
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
   *  When true, the radio button will be rendered as a switch.
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
  public Boolean isTabTraversable() {
    if (this.ctrl != null) {
      try {
        bbjRadioButton.isTabTraversable();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.tabTraversable;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public RadioButton setTabTraversable(Boolean traverse) {
    if (this.ctrl != null) {
      try {
        bbjRadioButton.setTabTraversable(traverse);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.tabTraversable = traverse;
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
    super.setControlExpanse(expanse);
    return this;
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

    if (this.alignment != Alignment.LEFT) {
      try {
        bbjRadioButton.setAlignment(alignment.alignment);
      } catch (BBjException e) {
        Environment.logError(e);
      }
      this.setAlignment(this.alignment);
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
      this.setChecked(this.checked);
    }

    if (Boolean.TRUE.equals(this.disabled)) {
      this.setEnabled(!this.disabled);
    }

    if (Boolean.TRUE.equals(this.valid)) {
      this.setValid(this.valid);
    }

    if (Boolean.TRUE.equals(this.required)) {
      this.setRequired(this.required);
    }

    if (Boolean.FALSE.equals(this.autoValidate)) {
      this.setAutoValidate(this.autoValidate);
    }

    if (Boolean.TRUE.equals(this.autoValidateOnLoad)) {
      this.setAutoValidateOnLoad(this.autoValidateOnLoad);
    }

    if (Boolean.TRUE.equals(this.autoWasValidated)) {
      this.setAutoWasValidate(this.autoWasValidated);
    }

  }

}
