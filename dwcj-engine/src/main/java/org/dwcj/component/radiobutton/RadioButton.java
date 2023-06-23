package org.dwcj.component.radiobutton;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import java.util.List;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.AbstractOptionInput;
import org.dwcj.component.checkbox.CheckBox;
import org.dwcj.component.window.AbstractWindow;
import org.dwcj.exceptions.DwcjRuntimeException;
import org.dwcj.utilities.BBjFunctionalityHelper;

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
public final class RadioButton extends AbstractOptionInput<RadioButton> {

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
     * Get the value of the activation.
     *
     * @return the value of the activation
     */
    public String getValue() {
      return this.value;
    }
  }

  private Activation activation;
  private RadioButtonGroup group = null;

  /**
   * Create a new radio button component.
   *
   * @param text Desired text for the radio button.
   * @param checked True if the radio button should be created as checked, false otherwise.
   */
  public RadioButton(String text, boolean checked) {
    super(text, checked);
  }

  /**
   * Create a new radio button component.
   *
   * @param text The text for the radio button.
   */
  public RadioButton(String text) {
    super(text);
  }

  /**
   * Create a new radio button component.
   */
  public RadioButton() {
    super();
  }

  /**
   * Configure if the radio button should be checked when it gains focus.
   *
   * @param value the activation type
   * @see Activation
   */
  public RadioButton setActivation(Activation value) {
    setProperty("activation", value.getValue());
    this.activation = value;

    return this;
  }

  /**
   * Get the activation type.
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
    if (Boolean.FALSE.equals(this.getCaughtUp()) && theGroup != null) {
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
   * Get the RadioButtonGroup which this RadioButton belongs to.
   *
   * @return the RadioButtonGroup which this RadioButton belongs to or null if this RadioButton
   *         doesn't belong to any group
   */
  public RadioButtonGroup getButtonGroup() {
    return this.group;
  }

  /**
   * Set the RadioButtonGroup which this RadioButton belongs to.
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
  protected void create(AbstractWindow p) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(p);
      byte[] flags =
          BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
      setControl(w.addRadioButton("", flags));
      this.catchUp();
    } catch (IllegalAccessException | BBjException e) {
      throw new DwcjRuntimeException("Failed to create the BBjRadioButton Control", e);
    }
  }
}
