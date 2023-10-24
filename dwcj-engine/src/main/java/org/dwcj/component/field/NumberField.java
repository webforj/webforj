package org.dwcj.component.field;

import org.dwcj.annotation.ExcludeFromJacocoGeneratedReport;
import org.dwcj.concern.HasHighlightOnFocus;
import org.dwcj.concern.HasHorizontalAlignment;
import org.dwcj.concern.HasMax;
import org.dwcj.concern.HasMin;
import org.dwcj.concern.HasPlaceholder;

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
public final class NumberField extends DwcFieldInitializer<NumberField, Double> implements
    HasMin<NumberField, Double>, HasMax<NumberField, Double>, HasPlaceholder<NumberField>,
    HasHighlightOnFocus<NumberField>, HasHorizontalAlignment<NumberField> {

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

    setComponentHorizontalAlignment(Alignment.RIGHT);
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
   * Specifies the granularity that the value must adhere. This effect is only applicable when using
   * the arrow keys to modify a value, and not for direct numeric input into the component itself.
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
  @Override
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
  @Override
  public String getPlaceholder() {
    return placeholder;
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
}
