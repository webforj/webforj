package com.webforj.component.field;

import com.basis.bbj.proxies.sysgui.BBjInputDSpinner;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.webforj.bridge.WindowAccessor;
import com.webforj.component.window.Window;
import com.webforj.exceptions.WebforjRuntimeException;
import com.webforj.utilities.BBjFunctionalityHelper;

/**
 * Represents a masked date field with an associated spinner.
 *
 * @author Hyyan Abo Fakher
 * @since 24.10
 *
 * @see MaskedDateField
 */
// We're purposefully ignoring the deep inheritance warning here because we've designed our class
// hierarchy to meet the unique requirements of our UI framework. This design closely aligns with
// our framework's specific goals and emphasizes the need for caution when considering any changes.
//
// Any changes to the inheritance structure should be thoughtfully evaluated in the context of our
// framework's needs. The current structure is essential for meeting those needs.
@SuppressWarnings("squid:S110")
public final class MaskedDateFieldSpinner extends MaskedDateField
    implements Spinnable<MaskedDateFieldSpinner> {
  private final SpinnableMixin spinnableMixin;
  private SpinField spinField = SpinField.DAY;

  /**
   * Defines the fields that can be spun.
   */
  public enum SpinField {
    /**
     * Spin by day.
     */
    DAY(BBjInputDSpinner.DAY),
    /**
     * Spin by week.
     */
    WEEK(BBjInputDSpinner.WEEK),
    /**
     * Spin by month.
     */
    MONTH(BBjInputDSpinner.MONTH),
    /**
     * Spin by year.
     */
    YEAR(BBjInputDSpinner.YEAR);

    private final int value;

    SpinField(int value) {
      this.value = value;
    }

    /**
     * Retrieves the value of the spin field.
     *
     * @return the value of the spin field
     */
    public int getValue() {
      return value;
    }
  }

  MaskedDateFieldSpinner(SpinnableMixin spinnableMixin) {
    super();
    this.spinnableMixin = spinnableMixin;
  }

  /**
   * Constructs a new masked date field spinner.
   */
  public MaskedDateFieldSpinner() {
    super();
    this.spinnableMixin = new SpinnableMixin(this);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public MaskedDateFieldSpinner spinUp() {
    spinnableMixin.spinUp();
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public MaskedDateFieldSpinner spinDown() {
    spinnableMixin.spinDown();
    return this;
  }

  /**
   * Sets the spin field.
   *
   * @param spinField the spin field
   * @return the component itself
   */
  public MaskedDateFieldSpinner setSpinField(SpinField spinField) {
    this.spinField = spinField;

    BBjInputDSpinner field = inferDateSpinnerField();
    if (field != null) {
      try {
        field.setSpinField(spinField.getValue());
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return this;
  }

  /**
   * Retrieves the spin field.
   *
   * @return the spin field
   */
  public SpinField getSpinField() {
    return spinField;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onAttach() {
    super.onAttach();

    if (spinField != null) {
      setSpinField(spinField);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onCreate(Window window) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(window);
      byte[] flags = BBjFunctionalityHelper.buildStandardCreationFlags(isVisible(), isEnabled());
      setControl(w.addInputDSpinner(flags));
    } catch (BBjException | IllegalAccessException e) {
      throw new WebforjRuntimeException("Failed to create BBjInputDSpinner", e);
    }
  }

  BBjInputDSpinner inferDateSpinnerField() {
    return (BBjInputDSpinner) inferField();
  }
}
