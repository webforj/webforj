package org.dwcj.component.field;

import org.dwcj.annotation.ExcludeFromJacocoGeneratedReport;
import org.dwcj.component.HasMax;
import org.dwcj.component.HasMin;
import org.dwcj.component.HasPlaceholder;
import org.dwcj.component.HighlightableOnFocus;

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
public final class NumberField extends AbstractField<NumberField, Double>
    implements HasMin<NumberField, Double>, HasMax<NumberField, Double>,
    HasPlaceholder<NumberField>, HighlightableOnFocus<NumberField> {

  private Double min = null;
  private Double max = null;
  private Double step = null;
  private String placeholder = null;

  /**
   * Construct a new text field with the given label and value.
   *
   * @param label the label for the field
   * @param value the value for the field
   */
  public NumberField(String label, Double value) {
    super();

    setUnrestrictedProperty("type", "number");
    setLabel(label);
    setValue(value);
  }

  /**
   * Construct a new number field with the given label.
   *
   * @param label the label for the field
   */
  public NumberField(String label) {
    this(label, null);
  }

  /**
   * Construct a new number field.
   */
  public NumberField() {
    this("");
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

    if (value == null || value.isEmpty()) {
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
   * Specifies the granularity that the value must adhere.
   *
   * @param step the step value for the granularity. If null, then no rules are forced.
   * @return the component
   */
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
  public Double getStep() {
    return step;
  }

  /**
   * Set the placeholder of field.
   *
   * @param placeholder the placeholder of field
   * @return the field type
   */
  public NumberField setPlaceholder(String placeholder) {
    this.placeholder = placeholder;
    setUnrestrictedProperty("placeholder", placeholder);
    return this;
  }

  /**
   * Get the placeholder of field.
   *
   * @return the placeholder of field
   */
  public String getPlaceholder() {
    return placeholder;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Behavior getHighlightOnFocus() {
    return getComponentHighlightOnFocus();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public NumberField setHighlightOnFocus(Behavior highlight) {
    setComponentHighlightOnFocus(highlight);
    return getSelf();
  }
}
