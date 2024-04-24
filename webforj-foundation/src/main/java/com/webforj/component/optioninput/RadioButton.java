package com.webforj.component.optioninput;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.webforj.bridge.WindowAccessor;
import com.webforj.component.window.Window;
import com.webforj.exceptions.WebforjRuntimeException;
import com.webforj.utilities.BBjFunctionalityHelper;
import java.util.Arrays;
import java.util.List;

/**
 * An implementation of a radio button -- an item that can be selected or deselected, and which
 * displays its state to the user. By convention, only one radio button in a group can be selected
 * at a time.
 *
 * <p>
 * When clicked, a radio button displays a filled circle inside it, indicating the selection (on).
 * When another radio button in the same group is clicked, the previously selected button is
 * deselected, and the new one becomes selected. Radio buttons are commonly used when mutually
 * exclusive options are available, allowing the user to choose a single option from a set of
 * choices.
 * </p>
 *
 * @see RadioButtonGroup
 * @see CheckBox
 *
 * @author Hyyan Abo Fakher
 * @since 23.01
 */
// We're purposefully ignoring the deep inheritance warning here because we've designed our class
// hierarchy to meet the unique requirements of our UI framework. This design closely aligns with
// our framework's specific goals and emphasizes the need for caution when considering any changes.
//
// Any changes to the inheritance structure should be thoughtfully evaluated in the context of our
// framework's needs. The current structure is essential for meeting those needs.
@SuppressWarnings("squid:S110")
public final class RadioButton extends DwcOptionInput<RadioButton> {

  /**
   * List of possible activation types supported by the radio button.
   */
  public enum Activation {
    /** Radio buttons will be checked when they gain focus for any reason. */
    AUTO("auto"),
    /**
     * Radio buttons will not be checked when they gain focus programmatically or by user keyboard
     * operation (LEFT, RIGHT, UP, DOWN arrows within a group).
     */
    MANUAL("manual");

    private final String value;

    private Activation(String value) {
      this.value = value;
    }

    /**
     * Gets the value of the activation.
     *
     * @return the value of the activation
     */
    public String getValue() {
      return this.value;
    }
  }

  private Activation activation = Activation.MANUAL;
  private RadioButtonGroup group = null;
  private boolean isSwitch = false;

  /**
   * Creates a new radio button component.
   *
   * @param name The name of the radio button.
   * @param text Desired text for the radio button.
   * @param checked True if the radio button should be created as checked, false otherwise.
   *
   * @since 24.01
   */
  public RadioButton(String name, String text, boolean checked) {
    super(text, checked);
    setName(name);
  }

  /**
   * Creates a new radio button component.
   *
   * @param name The name of the radio button.
   * @param text The text for the radio button.
   *
   * @since 24.01
   */
  public RadioButton(String name, String text) {
    this(name, text, false);
  }

  /**
   * Creates a new radio button component.
   *
   * @param text The text for the radio button.
   * @param checked True if the radio button should be created as checked, false otherwise.
   */
  public RadioButton(String text, boolean checked) {
    this(text, text, checked);
  }

  /**
   * Creates a new radio button component with a random name.
   *
   * @param text The text for the radio button.
   */
  public RadioButton(String text) {
    this(text, text, false);
  }

  /**
   * Creates a new radio button component with a random name.
   */
  public RadioButton() {
    this("", false);
  }

  /**
   * Factory method to create a radio button which is rendered as a switch button.
   *
   * @param name The name of the radio button.
   * @param text The text for the radio button.
   * @param checked True if the radio button should be created as checked, false otherwise.
   *
   * @return The created radio button with the switch style.
   *
   * @since 24.01
   */
  public static RadioButton Switch(String name, String text, boolean checked) { // NOSONAR
    return new RadioButton(name, text, checked).setSwitch(true);
  }

  /**
   * Factory method to create a radio button which is rendered as a switch button.
   *
   * @param name The name of the radio button.
   * @param text The text for the radio button.
   *
   * @return The created radio button with the switch style.
   *
   * @since 24.01
   */
  public static RadioButton Switch(String name, String text) { // NOSONAR
    return RadioButton.Switch(name, text, false);
  }

  /**
   * Factory method to create a radio button which is rendered as a switch button.
   *
   * @param text The text for the radio button.
   * @param checked True if the radio button should be created as checked, false otherwise.
   *
   * @return The created radio button with the switch style.
   * @since 23.02
   */
  public static RadioButton Switch(String text, boolean checked) { // NOSONAR
    return new RadioButton(text, checked).setSwitch(true);
  }

  /**
   * Factory method to create a radio button which is rendered as a switch button.
   *
   * @param text The text for the radio button.
   *
   * @return The created radio button with the switch style.
   * @since 23.02
   */
  public static RadioButton Switch(String text) { // NOSONAR
    return new RadioButton(text).setSwitch(true);
  }

  /**
   * Factory method to create a radio button which is rendered as a switch button.
   *
   * @return The created radio button with the switch style.
   * @since 23.02
   */
  public static RadioButton Switch() { // NOSONAR
    return new RadioButton().setSwitch(true);
  }

  /**
   * Configures if the radio button should be checked when it gains focus.
   *
   * @param value the activation type
   * @see Activation
   */
  public RadioButton setActivation(Activation value) {
    setUnrestrictedProperty("activation", value.getValue());
    this.activation = value;

    return this;
  }

  /**
   * Gets the activation type.
   *
   * @return the activation type
   */
  public Activation getActivation() {
    return this.activation;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isChecked() {
    // this method should return false if radio button
    // belongs to a group and there is a button already checked

    RadioButtonGroup theGroup = getButtonGroup();
    if (Boolean.FALSE.equals(this.isAttached()) && theGroup != null) {
      List<RadioButton> checkedButtons =
          theGroup.getButtons().stream().filter(RadioButton::getChecked).toList();

      if (checkedButtons.size() > 1) {
        return checkedButtons.get(checkedButtons.size() - 1) == this;
      }
    }

    return super.isChecked();
  }

  /**
   * Same as {@link #isChecked()} but this method doesn't check if the RadioButton can be checked
   * base on the RadioButtonGroup it belongs to.
   *
   * @y.exclude
   */
  boolean getChecked() {
    return super.isChecked();
  }

  /**
   * Gets the RadioButtonGroup which this RadioButton belongs to.
   *
   * @return the RadioButtonGroup which this RadioButton belongs to or null if this RadioButton
   *         doesn't belong to any group
   */
  public RadioButtonGroup getButtonGroup() {
    return this.group;
  }

  /**
   * Sets the RadioButtonGroup which this RadioButton belongs to.
   *
   * @param group the RadioButtonGroup which this RadioButton belongs to
   */
  void setButtonGroup(RadioButtonGroup group) {
    this.group = group;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<String> getRestrictedProperties() {
    List<String> properties = super.getRestrictedProperties();
    properties.addAll(Arrays.asList("activation", "autoValidate", "autoValidateOnLoad",
        "autoWasValidated", "checked", "disabled", "expanse", "hasFocus", "invalid",
        "invalidMessage", "label", "name", "readonly", "required", "switch", "tabTraversable",
        "valid", "validationIcon", "validationPopoverDistance", "validationPopoverPlacement",
        "validationPopoverSkidding", "validationStyle", "validator", "value"));

    return properties;
  }

  /**
   * Renders the radio button as a switch.
   *
   * <p>
   * A switch component is a user interface element that represents a binary choice, such as turning
   * something on or off. It visually resembles a physical switch that can be toggled between two
   * states. It provides a clear visual indication of the current state, making it easy for users to
   * understand and interact with the switch.
   * </p>
   *
   * @param isSwitch When true, the radio button will be rendered as a switch
   * @since 23.02
   */
  public RadioButton setSwitch(boolean isSwitch) {
    this.isSwitch = isSwitch;
    setUnrestrictedProperty("switch", isSwitch);
    return this;
  }

  /**
   * Checks if the radio button is rendered as a switch.
   *
   * @return true when the radio button is rendered as a switch, false otherwise.
   *
   * @see #setSwitch(boolean)
   * @since 23.02
   */
  public boolean isSwitch() {
    return isSwitch;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onCreate(Window p) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(p);
      byte[] flags =
          BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
      setControl(w.addRadioButton("", flags));
    } catch (IllegalAccessException | BBjException e) {
      throw new WebforjRuntimeException("Failed to create the BBjRadioButton Control", e);
    }
  }
}
