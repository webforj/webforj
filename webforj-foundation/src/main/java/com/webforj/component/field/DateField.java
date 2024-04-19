package com.webforj.component.field;

import com.webforj.annotation.ExcludeFromJacocoGeneratedReport;
import com.webforj.concern.HasHighlightOnFocus;
import com.webforj.concern.HasMax;
import com.webforj.concern.HasMin;
import java.time.LocalDate;

/**
 * The DateField provides a user interface component that let the user enter a date, either with a
 * field that validates the input or a special date picker interface.
 *
 * <p>
 * The resulting value includes the year, month, and day, but not the time. The displayed date is
 * formatted based on the locale of the user's browser but the parsed value is always formatted
 * yyyy-MM-dd.
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
public final class DateField extends DwcFieldInitializer<DateField, LocalDate> implements
    HasMin<DateField, LocalDate>, HasMax<DateField, LocalDate>, HasHighlightOnFocus<DateField> {

  private LocalDate min = null;
  private LocalDate max = null;

  /**
   * Construct a new DateField with the given label and value.
   *
   * @param label the label for the field
   * @param date the initial value for the field
   */
  public DateField(String label, LocalDate date) {
    super();

    setUnrestrictedProperty("type", "date");
    setLabel(label);
    setValue(date);
  }

  /**
   * Construct a new DateField with the given label.
   *
   * @param label the label for the field
   */
  public DateField(String label) {
    this(label, null);
  }

  /**
   * Construct a new DateField with the given value.
   *
   * @param date the initial value for the field
   */
  public DateField(LocalDate date) {
    this("", date);
  }

  /**
   * Construct a new DateField.
   */
  public DateField() {
    this("", null);
  }

  /**
   * Set The latest date to accept.
   *
   * <p>
   * If the value entered into the component occurs afterward, the component fails constraint
   * validation. If both the max and min values are set, this value must be a date later than or
   * equal to the one in the min attribute.
   * </p>
   *
   * @param max the maximum date to accept
   * @return the component itself
   */
  @Override
  public DateField setMax(LocalDate max) {
    LocalDate currentValue = getValue();

    if (max != null && currentValue != null && max.isBefore(currentValue)) {
      throw new IllegalArgumentException(
          "Maximum date must be later than or equal to the current value");
    }

    if (min != null && max != null && max.isBefore(min)) {
      throw new IllegalArgumentException(
          "Maximum date must be later than or equal to the minimum date");
    }

    this.max = max;
    setUnrestrictedProperty("max", max == null ? null : DateField.toDate(max));
    return this;
  }

  /**
   * Get the latest date to accept.
   *
   * @return the maximum date to accept
   */
  @Override
  public LocalDate getMax() {
    return max;
  }

  /**
   * Set The earliest date to accept.
   *
   * <p>
   * If the value entered into the component occurs beforehand, the element fails constraint
   * validation. If both the max and min attributes are set, this value must be a date earlier than
   * or equal to max.
   * </p>
   */
  @Override
  public DateField setMin(LocalDate min) {
    LocalDate currentValue = getValue();

    if (min != null && currentValue != null && min.isAfter(currentValue)) {
      throw new IllegalArgumentException(
          "Minimum date must be earlier than or equal to the current value");
    }

    if (max != null && min != null && max.isBefore(min)) {
      throw new IllegalArgumentException(
          "Minimum date must be earlier than or equal to the maximum date");
    }

    this.min = min;
    setUnrestrictedProperty("min", min == null ? null : DateField.toDate(min));
    return this;
  }

  /**
   * Get the earliest date to accept.
   *
   * @return the minimum date to accept
   */
  @Override
  public LocalDate getMin() {
    return min;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public DateField setText(String text) {
    if (text != null && !text.isEmpty() && !DateField.isValidDate(text)) {
      throw new IllegalArgumentException("Text must be a valid date in yyyy-MM-dd format");
    }

    if (min != null && text != null && LocalDate.parse(text).isBefore(min)) {
      throw new IllegalArgumentException("Date must be later than or equal to the minimum date");
    }

    if (max != null && text != null && LocalDate.parse(text).isAfter(max)) {
      throw new IllegalArgumentException("Date must be earlier than or equal to the maximum date");
    }

    super.setText(text);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public DateField setValue(LocalDate value) {
    setText(value == null ? null : DateField.toDate(value));
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public LocalDate getValue() {
    String text = getText();
    return text == null || text.isEmpty() ? null : DateField.fromDate(text);
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
  public DateField setHighlightOnFocus(Behavior highlight) {
    setComponentHighlightOnFocus(highlight);
    return getSelf();
  }

  /**
   * Convert a date string in yyyy-MM-dd format to a LocalDate.
   *
   * @param dateAsString the date string in yyyy-MM-dd format
   * @return the LocalDate
   */
  public static LocalDate fromDate(String dateAsString) {
    return LocalDate.parse(dateAsString);
  }

  /**
   * Convert a LocalDate to a date string in yyyy-MM-dd format.
   *
   * @param date the LocalDate
   * @return the date string in yyyy-MM-dd format
   */
  public static String toDate(LocalDate date) {
    return date.toString();
  }

  /**
   * Check if the given string is valid yyyy-MM-dd date.
   *
   * @param dateAsString the date string
   * @return true if the string is a valid date, false otherwise
   */

  public static boolean isValidDate(String dateAsString) {
    try {
      LocalDate.parse(dateAsString);
      return true;
    } catch (Exception e) {
      return false;
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected LocalDate convertValue(String value) {
    return DateField.fromDate(value);
  }
}
