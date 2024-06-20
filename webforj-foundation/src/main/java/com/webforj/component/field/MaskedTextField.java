package com.webforj.component.field;

import java.util.Arrays;
import java.util.List;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.webforj.MaskDecorator;
import com.webforj.bridge.WindowAccessor;
import com.webforj.component.window.Window;
import com.webforj.concern.HasPattern;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventListener;
import com.webforj.exceptions.WebforjRuntimeException;
import com.webforj.utilities.BBjFunctionalityHelper;

/**
 * Represents a masked text field.
 *
 * <p>
 * The masked text field is a text field that allows the user to enter text in a specific format.
 * The format is defined by a mask which is a string that contains the characters that the user can
 * enter.
 * </p>
 *
 * <p>
 * The mask format is as follows:
 * </p>
 *
 * <table border="1">
 * <caption>Mask Format</caption>
 * <tr>
 * <th>Mask Character</th>
 * <th>Accepts</th>
 * </tr>
 * <tr>
 * <td>X</td>
 * <td>Any printable character.</td>
 * </tr>
 * <tr>
 * <td>a</td>
 * <td>Any alphabetic character.</td>
 * </tr>
 * <tr>
 * <td>A</td>
 * <td>Any alphabetic character. Converts lower-case alphabetic characters to upper case.</td>
 * </tr>
 * <tr>
 * <td>0</td>
 * <td>Any digit.</td>
 * </tr>
 * <tr>
 * <td>z</td>
 * <td>Any digit or alphabetic character.</td>
 * </tr>
 * <tr>
 * <td>Z</td>
 * <td>Any digit or alphabetic character. Converts lower-case alphabetic characters to upper
 * case.</td>
 * </tr>
 * </table>
 *
 * <p>
 * Any other character in the mask represents itself. If a character type mismatch occurs, such as
 * trying to place a "W" into a mask position designated for a digit, the input is ignored. If the
 * source string is too short, spaces are used to pad the remainder of the mask. If the source
 * string is too long, it is truncated.
 * </p>
 *
 *
 * @author Hyyan Abo Fakher
 * @since 24.10
 */
// We're purposefully ignoring the deep inheritance warning here because we've designed our class
// hierarchy to meet the unique requirements of our UI framework. This design closely aligns with
// our framework's specific goals and emphasizes the need for caution when considering any changes.
//
// Any changes to the inheritance structure should be thoughtfully evaluated in the context of our
// framework's needs. The current structure is essential for meeting those needs.
@SuppressWarnings("squid:S110")
public sealed class MaskedTextField extends DwcMaskedField<MaskedTextField, String>
    implements HasPattern<MaskedTextField> permits MaskedTextFieldSpinner {
  static final String DEFAULT_MASK =
      "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
          + "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
          + "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
          + "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX";

  private String pattern = null;

  /**
   * Constructs a new masked text field with a label, value, and placeholder.
   *
   * @param label the label of the field
   * @param value the value of the field
   * @param placeholder the placeholder of the field
   */
  public MaskedTextField(String label, String value, String placeholder) {
    super(label, value, placeholder);
    postInit();
  }

  /**
   * Constructs a new masked text field with a label, value, and a value change listener.
   *
   * @param label the label of the field
   * @param value the value of the field
   * @param listener the value change listener
   */
  public MaskedTextField(String label, String value,
      EventListener<ValueChangeEvent<String>> listener) {
    super(label, value, listener);
    postInit();
  }

  /**
   * Constructs a new masked text field with a label and value.
   *
   * @param label the label of the field
   * @param value the value of the field
   */
  public MaskedTextField(String label, String value) {
    super(label, value);
    postInit();
  }

  /**
   * Constructs a new masked text field with a label and a value change listener.
   *
   * @param label the label of the field
   * @param listener the value change listener
   */
  public MaskedTextField(String label, EventListener<ValueChangeEvent<String>> listener) {
    super(label, listener);
    postInit();
  }

  /**
   * Constructs a new masked text field with a value change listener.
   *
   * @param listener the value change listener
   */
  public MaskedTextField(EventListener<ValueChangeEvent<String>> listener) {
    super(listener);
    postInit();
  }

  /**
   * Constructs a new masked text field with a label.
   *
   * @param label the label of the field
   */
  public MaskedTextField(String label) {
    super(label);
    postInit();
  }

  /**
   * Constructs a new masked text field.
   */
  public MaskedTextField() {
    super();
    postInit();
  }

  private void postInit() {
    setMask(DEFAULT_MASK);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public MaskedTextField setValue(String value) {
    setText(value);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getValue() {
    return getText();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getMaskedValue() {
    String value = getValue();
    if (value == null) {
      return "";
    }

    return MaskDecorator.forString(value, getMask());
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public MaskedTextField setPattern(String pattern) {
    this.pattern = pattern;
    setUnrestrictedProperty("pattern", pattern);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getPattern() {
    return pattern;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<String> getRestrictedProperties() {
    List<String> properties = super.getRestrictedProperties();
    properties.addAll(Arrays.asList("autoValidate", "autoValidateOnLoad", "autoWasValidated",
        "autocomplete", "autocorrect", "autofocus", "disabled", "expanse", "hasFocus",
        "highlightBehaviors", "insertMode", "invalid", "invalidMessage", "label", "mask",
        "maskedValue", "name", "padCharacter", "placeholder", "readonly", "required",
        "restoreValue", "showSpinners", "spellcheck", "spinnable", "tabTraversable", "valid",
        "validationIcon", "validationPopoverDistance", "validationPopoverPlacement",
        "validationPopoverSkidding", "validationStyle", "validator", "value"));

    return properties;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onCreate(Window window) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(window);
      byte[] flags = BBjFunctionalityHelper.buildStandardCreationFlags(isVisible(), isEnabled());
      setControl(w.addInputE(flags));
    } catch (BBjException | IllegalAccessException e) {
      throw new WebforjRuntimeException("Failed to create BBjInputE", e);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected String convertValue(String value) {
    return value;
  }
}
