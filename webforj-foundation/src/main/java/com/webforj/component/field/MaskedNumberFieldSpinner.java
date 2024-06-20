package com.webforj.component.field;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.webforj.bridge.WindowAccessor;
import com.webforj.component.window.Window;
import com.webforj.concern.HasStep;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventListener;
import com.webforj.exceptions.WebforjRuntimeException;
import com.webforj.utilities.BBjFunctionalityHelper;
import java.util.Objects;

/**
 * Represents a masked number field with an associated spinner.
 *
 * @author Hyyan Abo Fakher
 * @since 24.10
 *
 * @see MaskedNumberField
 */
// We're purposefully ignoring the deep inheritance warning here because we've designed our class
// hierarchy to meet the unique requirements of our UI framework. This design closely aligns with
// our framework's specific goals and emphasizes the need for caution when considering any changes.
//
// Any changes to the inheritance structure should be thoughtfully evaluated in the context of our
// framework's needs. The current structure is essential for meeting those needs.
@SuppressWarnings("squid:S110")
public final class MaskedNumberFieldSpinner extends MaskedNumberField
    implements HasStep<MaskedNumberFieldSpinner, Double>, Spinnable<MaskedNumberFieldSpinner> {
  private SpinnableMixin spinnableMixin;
  private Double step = null;

  /**
   * Constructs a new masked number field spinner with a label, value, and placeholder.
   *
   * @param label the label of the field
   * @param value the value of the field
   * @param placeholder the placeholder of the field
   */
  public MaskedNumberFieldSpinner(String label, Double value, String placeholder) {
    super(label, value, placeholder);
    postInit();
  }

  /**
   * Constructs a new masked number field spinner with a label, value, and a value change listener.
   *
   * @param label the label of the field
   * @param value the value of the field
   * @param listener the value change listener
   */
  public MaskedNumberFieldSpinner(String label, Double value,
      EventListener<ValueChangeEvent<Double>> listener) {
    super(label, value, listener);
    postInit();
  }

  /**
   * Constructs a new masked number field spinner with a label and value.
   *
   * @param label the label of the field
   * @param value the value of the field
   */
  public MaskedNumberFieldSpinner(String label, Double value) {
    super(label, value);
    postInit();
  }

  /**
   * Constructs a new masked number field spinner with a label and a value change listener.
   *
   * @param label the label of the field
   * @param listener the value change listener
   */
  public MaskedNumberFieldSpinner(String label, EventListener<ValueChangeEvent<Double>> listener) {
    super(label, listener);
    postInit();
  }

  /**
   * Constructs a new masked number field spinner with a value change listener.
   *
   * @param listener the value change listener
   */
  public MaskedNumberFieldSpinner(EventListener<ValueChangeEvent<Double>> listener) {
    super(listener);
    postInit();
  }

  /**
   * Constructs a new masked number field spinner with a label.
   *
   * @param label the label of the field
   */
  public MaskedNumberFieldSpinner(String label) {
    super(label);
    postInit();
  }

  /**
   * Constructs a new masked number field.
   */
  public MaskedNumberFieldSpinner() {
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
  public MaskedNumberFieldSpinner setStep(Double step) {
    Objects.requireNonNull(step, "Step cannot be null");
    this.step = step;
    setUnrestrictedProperty("step", step);

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Double getStep() {
    return step;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public MaskedNumberFieldSpinner spinUp() {
    spinnableMixin.spinUp();
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public MaskedNumberFieldSpinner spinDown() {
    spinnableMixin.spinDown();
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onCreate(Window window) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(window);
      byte[] flags = BBjFunctionalityHelper.buildStandardCreationFlags(isVisible(), isEnabled());
      setControl(w.addInputNSpinner(flags));
    } catch (BBjException | IllegalAccessException e) {
      throw new WebforjRuntimeException("Failed to create BBjInputNSpinner", e);
    }
  }
}
