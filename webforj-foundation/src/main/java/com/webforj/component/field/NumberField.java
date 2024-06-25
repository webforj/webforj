package com.webforj.component.field;

import com.webforj.annotation.ExcludeFromJacocoGeneratedReport;
import com.webforj.concern.HasHorizontalAlignment;
import com.webforj.concern.HasMax;
import com.webforj.concern.HasMin;
import com.webforj.concern.HasStep;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventListener;

/**
 * The NumberField provides a user interface component let the user enter a number. They include
 * built-in validation to reject non-numerical entries.
 *
 * <p>
 * The browser may opt to provide stepper arrows to let the user increase and decrease the value
 * using their mouse or by tapping with a fingertip.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
// We're purposefully ignoring the deep inheritance warning here because we've designed our class
// hierarchy to meet the unique requirements of our UI framework. This design closely aligns with
// our framework's specific goals and emphasizes the need for caution when considering any changes.
//
// Any changes to the inheritance structure should be thoughtfully evaluated in the context of our
// framework's needs. The current structure is essential for meeting those needs.
@SuppressWarnings("squid:S110")
public final class NumberField extends DwcFieldInitializer<NumberField, Double>
    implements HasMin<NumberField, Double>, HasMax<NumberField, Double>,
    HasStep<NumberField, Double>, HasHorizontalAlignment<NumberField> {

  private Double min = null;
  private Double max = null;
  private Double step = null;

  /**
   * Constructs a new NumberField with a label, value, and placeholder.
   *
   * @param label the label of the field
   * @param value the value of the field
   * @param placeholder the placeholder of the field
   */
  public NumberField(String label, Double value, String placeholder) {
    super(label, value, placeholder);
    postInit();
  }

  /**
   * Constructs a new NumberField with a label, value, and a value change listener.
   *
   * @param label the label of the field
   * @param value the value of the field
   * @param listener the value change listener
   */
  public NumberField(String label, Double value, EventListener<ValueChangeEvent<Double>> listener) {
    super(label, value, listener);
    postInit();
  }

  /**
   * Constructs a new PasswordField with a label and value.
   *
   * @param label the label of the field
   * @param value the value of the field
   */
  public NumberField(String label, Double value) {
    super(label, value);
    postInit();
  }

  /**
   * Constructs a new NumberField with a label and a value change listener.
   *
   * @param label the label of the field
   * @param listener the value change listener
   */
  public NumberField(String label, EventListener<ValueChangeEvent<Double>> listener) {
    super(label, listener);
    postInit();
  }

  /**
   * Constructs a new NumberField with a value change listener.
   *
   * @param listener the value change listener
   */
  public NumberField(EventListener<ValueChangeEvent<Double>> listener) {
    super(listener);
    postInit();
  }

  /**
   * Constructs a new NumberField with a label.
   *
   * @param label the label of the field
   */
  public NumberField(String label) {
    super(label);
    postInit();
  }

  /**
   * Constructs a new NumberField.
   */
  public NumberField() {
    super();
    postInit();
  }

  private void postInit() {
    setComponentHorizontalAlignment(Alignment.RIGHT);
    setUnrestrictedProperty("type", "number");
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public NumberField setValue(Double value) {
    return setText(String.valueOf(value));
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Double getValue() {
    String value = getText();

    if (value == null || value.isEmpty() || "null".equals(value)) {
      return null;
    }

    return Double.valueOf(getText());
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public NumberField setMax(Double max) {
    this.max = max;
    setUnrestrictedProperty("max", max);

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Double getMax() {
    return max;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public NumberField setMin(Double min) {
    this.min = min;
    setUnrestrictedProperty("min", min);

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Double getMin() {
    return min;
  }

  /**
   * Specifies the granularity that the value must adhere. This effect is only applicable when using
   * the arrow keys to modify a value, and not for direct numeric input into the component itself.
   *
   * @param step the step value for the granularity. If null, then no rules are forced.
   * @return the component
   */
  @Override
  public NumberField setStep(Double step) {
    this.step = step;
    setUnrestrictedProperty("step", step == null ? "any" : step);

    return this;
  }

  /**
   * Get the step value for the granularity.
   *
   * @return the step value for the granularity
   */
  @Override
  public Double getStep() {
    return step;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public NumberField setHorizontalAlignment(Alignment alignment) {
    setComponentHorizontalAlignment(alignment);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Alignment getHorizontalAlignment() {
    return getComponentHorizontalAlignment();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected Double convertValue(String value) {
    try {
      return Double.valueOf(value);
    } catch (NumberFormatException e) {
      return null;
    }
  }
}
