package com.webforj.component.optioninput;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.webforj.bridge.WindowAccessor;
import com.webforj.component.window.Window;
import com.webforj.exceptions.DwcjRuntimeException;
import com.webforj.utilities.BBjFunctionalityHelper;
import java.util.Arrays;
import java.util.List;

/**
 * An implementation of a check box -- an item that can be selected or deselected, and which
 * displays its state to the user. By convention, any number of check boxes in a group can be
 * selected.
 *
 * <p>
 * When clicked, a check mark appears inside the box, to indicate an affirmative choice (on). When
 * clicked again, the check mark disappears, indicating a negative choice (off). Check boxes are
 * used when more than one option may need to be checked or as an easy way to enable or disable a
 * setting in a software program.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 23.01
 */
public final class CheckBox extends DwcOptionInput<CheckBox> {

  private boolean indeterminate;

  /**
   * Creates a new checkbox component.
   *
   * @param text Desired text for the checkbox.
   * @param checked True if the checkbox should be created as checked, false otherwise.
   */
  public CheckBox(String text, boolean checked) {
    super(text, checked);
  }

  /**
   * Creates a new checkbox component.
   *
   * @param text The text for the checkbox.
   */
  public CheckBox(String text) {
    this(text, false);
  }

  /**
   * Creates a new checkbox component.
   */
  public CheckBox() {
    this("");
  }

  /**
   * Sets the checkbox to be indeterminate.
   *
   * @param value When true then the checkbox's value is neither true nor false, but is instead
   *        indeterminate, meaning that its state cannot be determined or stated in pure binary
   *        terms.
   *
   * @return The checkbox itself
   */
  public CheckBox setIndeterminate(boolean value) {
    setUnrestrictedProperty("indeterminate", value);
    this.indeterminate = value;
    return this;
  }

  /**
   * Returns whether or not the checkbox is indeterminate.
   *
   * @return A Boolean representing whether or not the checkbox is indeterminate.
   */
  public boolean isIndeterminate() {
    return this.indeterminate;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<String> getRestrictedProperties() {
    List<String> properties = super.getRestrictedProperties();
    properties.addAll(Arrays.asList("autoValidate", "autoValidateOnLoad", "autoWasValidated",
        "checked", "disabled", "expanse", "hasFocus", "indeterminate", "invalid", "invalidMessage",
        "label", "name", "readonly", "required", "tabTraversable", "valid", "validationIcon",
        "validationPopoverDistance", "validationPopoverPlacement", "validationPopoverSkidding",
        "validationStyle", "validator", "value"));

    return properties;
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
      setControl(w.addCheckBox("", flags));
    } catch (IllegalAccessException | BBjException e) {
      throw new DwcjRuntimeException("Failed to create the BBjCheckBox Control", e);
    }
  }
}
