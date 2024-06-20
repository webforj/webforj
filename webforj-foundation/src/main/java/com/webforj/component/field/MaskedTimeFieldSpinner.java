package com.webforj.component.field;

import com.basis.bbj.proxies.sysgui.BBjInputTSpinner;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.webforj.bridge.WindowAccessor;
import com.webforj.component.window.Window;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventListener;
import com.webforj.exceptions.WebforjRuntimeException;
import com.webforj.utilities.BBjFunctionalityHelper;
import java.time.LocalTime;

/**
 * Represents a masked time field with an associated spinner.
 *
 * @author Hyyan Abo Fakher
 * @since 24.10
 *
 * @see MaskedTimeField
 */
// We're purposefully ignoring the deep inheritance warning here because we've designed our class
// hierarchy to meet the unique requirements of our UI framework. This design closely aligns with
// our framework's specific goals and emphasizes the need for caution when considering any changes.
//
// Any changes to the inheritance structure should be thoughtfully evaluated in the context of our
// framework's needs. The current structure is essential for meeting those needs.
@SuppressWarnings("squid:S110")
public final class MaskedTimeFieldSpinner extends MaskedTimeField
    implements Spinnable<MaskedTimeFieldSpinner> {
  private SpinnableMixin spinnableMixin;
  private SpinField spinField = SpinField.HOUR;

  /**
   * Defines the fields that can be spun.
   */
  public enum SpinField {
    /**
     * Spin by hour.
     */
    HOUR(BBjInputTSpinner.HOUR),
    /**
     * Spin by minute.
     */
    MINUTE(BBjInputTSpinner.MINUTE),
    /**
     * Spin by second.
     */
    SECOND(BBjInputTSpinner.SECOND),

    /**
     * Spin by millisecond.
     */
    MILLISECOND(BBjInputTSpinner.MILLISECOND);

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
   * Constructs a new masked time field spinner with a label, value, and placeholder.
   *
   * @param label the label of the field
   * @param value the value of the field
   * @param placeholder the placeholder of the field
   */
  public MaskedTimeFieldSpinner(String label, LocalTime value, String placeholder) {
    super(label, value, placeholder);
    postInit();
  }

  /**
   * Constructs a new masked time field spinner with a label, value, and a value change listener.
   *
   * @param label the label of the field
   * @param value the value of the field
   * @param listener the value change listener
   */
  public MaskedTimeFieldSpinner(String label, LocalTime value,
      EventListener<ValueChangeEvent<LocalTime>> listener) {
    super(label, value, listener);
    postInit();
  }

  /**
   * Constructs a new masked time field spinner with a label and value.
   *
   * @param label the label of the field
   * @param value the value of the field
   */
  public MaskedTimeFieldSpinner(String label, LocalTime value) {
    super(label, value);
    postInit();
  }

  /**
   * Constructs a new masked time field spinner with a label and a value change listener.
   *
   * @param label the label of the field
   * @param listener the value change listener
   */
  public MaskedTimeFieldSpinner(String label, EventListener<ValueChangeEvent<LocalTime>> listener) {
    super(label, listener);
    postInit();
  }

  /**
   * Constructs a new masked time field spinner with a value change listener.
   *
   * @param listener the value change listener
   */
  public MaskedTimeFieldSpinner(EventListener<ValueChangeEvent<LocalTime>> listener) {
    super(listener);
    postInit();
  }

  /**
   * Constructs a new masked time field spinner with a label.
   *
   * @param label the label of the field
   */
  public MaskedTimeFieldSpinner(String label) {
    super(label);
    postInit();
  }

  /**
   * Constructs a new masked time field spinner.
   */
  public MaskedTimeFieldSpinner() {
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
  public MaskedTimeFieldSpinner spinUp() {
    spinnableMixin.spinUp();
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public MaskedTimeFieldSpinner spinDown() {
    spinnableMixin.spinDown();
    return this;
  }

  /**
   * Sets the field to be spun.
   *
   * <p>
   * The spin field must correspond to a time value in the mask. For example, if the mask is "%h:%mz
   * %a", the spin field should be HOUR or MINUTE, not SECOND or MILLISECOND.
   * </p>
   *
   * @param spinField The field to be spun.
   * @return This component itself.
   */
  public MaskedTimeFieldSpinner setSpinField(SpinField spinField) {
    this.spinField = spinField;

    BBjInputTSpinner field = inferDateSpinnerField();
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
      setControl(w.addInputTSpinner(flags));
    } catch (BBjException | IllegalAccessException e) {
      throw new WebforjRuntimeException("Failed to create BBjInputTSpinner", e);
    }
  }

  BBjInputTSpinner inferDateSpinnerField() {
    return (BBjInputTSpinner) inferField();
  }
}
