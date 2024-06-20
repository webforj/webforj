package com.webforj.component.field;

import com.basis.bbj.proxies.sysgui.BBjInputDSpinner;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.webforj.bridge.WindowAccessor;
import com.webforj.component.window.Window;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventListener;
import com.webforj.exceptions.WebforjRuntimeException;
import com.webforj.utilities.BBjFunctionalityHelper;
import java.time.LocalDate;

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
  private SpinnableMixin spinnableMixin;
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

  /**
   * Constructs a new masked field with a label, value, and placeholder.
   *
   * @param label the label of the field
   * @param value the value of the field
   * @param placeholder the placeholder of the field
   */
  public MaskedDateFieldSpinner(String label, LocalDate value, String placeholder) {
    super(label, value, placeholder);
    postInit();
  }

  /**
   * Constructs a new masked field with a label, value, and a value change listener.
   *
   * @param label the label of the field
   * @param value the value of the field
   * @param listener the value change listener
   */
  public MaskedDateFieldSpinner(String label, LocalDate value,
      EventListener<ValueChangeEvent<LocalDate>> listener) {
    super(label, value, listener);
    postInit();
  }

  /**
   * Constructs a new masked field with a label and value.
   *
   * @param label the label of the field
   * @param value the value of the field
   */
  public MaskedDateFieldSpinner(String label, LocalDate value) {
    super(label, value);
    postInit();
  }

  /**
   * Constructs a new masked field with a label and a value change listener.
   *
   * @param label the label of the field
   * @param listener the value change listener
   */
  public MaskedDateFieldSpinner(String label, EventListener<ValueChangeEvent<LocalDate>> listener) {
    super(label, listener);
    postInit();
  }

  /**
   * Constructs a new masked field with a value change listener.
   *
   * @param listener the value change listener
   */
  public MaskedDateFieldSpinner(EventListener<ValueChangeEvent<LocalDate>> listener) {
    super(listener);
    postInit();
  }

  /**
   * Constructs a new masked field with a label.
   *
   * @param label the label of the field
   */
  public MaskedDateFieldSpinner(String label) {
    super(label);
    postInit();
  }

  /**
   * Constructs a new masked field.
   */
  public MaskedDateFieldSpinner() {
    super();
    postInit();
  }

  private void postInit() { // NOSONAR
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
