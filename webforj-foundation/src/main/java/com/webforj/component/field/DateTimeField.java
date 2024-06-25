package com.webforj.component.field;

import com.webforj.concern.HasMax;
import com.webforj.concern.HasMin;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventListener;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;

/**
 * The DateTimeField provides a user interface component that allows the user to enter both a date
 * and a time, including the year, month, and day as well as the time in hours and minutes, either
 * with a field that validates the input or a special date-time picker interface.
 *
 * <p>
 * The component's UI varies in general from browser to browser. In browsers with no support, these
 * degrade gracefully to simple field component.
 *
 * The component is intended to represent a local date and time, not necessarily the user's local
 * date and time. In other words, an implementation should allow any valid combination of year,
 * month, day, hour, and minute—even if such a combination is invalid in the user's local time zone
 * (such as times within a daylight saving time spring-forward transition gap). Some mobile browsers
 * (particularly on iOS) do not currently implement this correctly.
 * </p>
 *
 * <p>
 * The displayed date and time formats differ from the actual value; the displayed date and time are
 * formatted according to the user's locale as reported by their operating system.
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
public final class DateTimeField extends DwcFieldInitializer<DateTimeField, LocalDateTime>
    implements HasMin<DateTimeField, LocalDateTime>, HasMax<DateTimeField, LocalDateTime> {

  private LocalDateTime min = null;
  private LocalDateTime max = null;

  /**
   * Constructs a new DateTimeField with a label, value, and a value change listener.
   *
   * @param label the label of the field
   * @param value the value of the field
   * @param listener the value change listener
   */
  public DateTimeField(String label, LocalDateTime value,
      EventListener<ValueChangeEvent<LocalDateTime>> listener) {
    super(label, value, listener);
    postInit();
  }

  /**
   * Constructs a new DateTimeField with a label and a value change listener.
   *
   * @param label the label of the field
   * @param listener the value change listener
   */
  public DateTimeField(String label, EventListener<ValueChangeEvent<LocalDateTime>> listener) {
    super(label, listener);
    postInit();
  }

  /**
   * Constructs a new DateTimeField with a value change listener.
   *
   * @param listener the value change listener
   */
  public DateTimeField(EventListener<ValueChangeEvent<LocalDateTime>> listener) {
    super(listener);
    postInit();
  }

  /**
   * Constructs a new DateTimeField with a label.
   *
   * @param label the label of the field
   */
  public DateTimeField(String label) {
    super(label);
    postInit();
  }

  /**
   * Construct a new DateTimeField with the given value.
   *
   * @param value the value for the field
   */
  public DateTimeField(LocalDateTime value) {
    super();
    setValue(value);
    postInit();
  }

  /**
   * Construct a new DateTimeField with the given label and value.
   *
   * @param label the label for the field
   * @param value the value for the field
   */
  public DateTimeField(String label, LocalDateTime value) {
    super(label, value);
    postInit();
  }

  /**
   * Constructs a new DateTimeField.
   */
  public DateTimeField() {
    super();
    postInit();
  }

  private void postInit() {
    setUnrestrictedProperty("type", "datetime-local");
  }

  /**
   * Set the latest date and time to accept.
   *
   * <p>
   * If the value entered into the component is later than this timestamp, the component fails
   * constraint validation. Also, the value must specify a datetime later than or equal to the one
   * specified by the min value.
   * </p>
   *
   * @param max the latest date and time to accept
   * @return the component itself
   */
  @Override
  public DateTimeField setMax(LocalDateTime max) {
    LocalDateTime currentValue = getValue();

    if (max != null && currentValue != null && compareDateTime(max, currentValue) < 0) {
      throw new IllegalArgumentException(
          "Maximum date and time must be later than or equal to the current value");
    }

    if (min != null && max != null && max.isBefore(min)) {
      throw new IllegalArgumentException(
          "Maximum date and time must be later than or equal to the minimum date and time");
    }

    this.max = max;
    setUnrestrictedProperty("max",
        max == null ? null : DateTimeField.toDateTime(max.truncatedTo(ChronoUnit.SECONDS)));
    return this;
  }

  /**
   * Get the latest date and time to accept.
   *
   * @return the latest date and time to accept
   */
  @Override
  public LocalDateTime getMax() {
    return max;
  }

  /**
   * Set the earliest date and time to accept.
   *
   * <p>
   * Datetime earlier than this will cause the component to fail constraint validation. This value
   * must specify a datetime earlier than or equal to the one specified by the max value.
   * </p>
   *
   * @param min the earliest date and time to accept
   * @return the component itself
   */
  @Override
  public DateTimeField setMin(LocalDateTime min) {
    LocalDateTime currentValue = getValue();

    if (min != null && currentValue != null && compareDateTime(min, currentValue) > 0) {
      throw new IllegalArgumentException(
          "Minimum date and time must be earlier than or equal to the current value");
    }

    if (max != null && min != null && max.isBefore(min)) {
      throw new IllegalArgumentException(
          "Minimum date and time must be earlier than or equal to the maximum date and time");
    }

    this.min = min;
    setUnrestrictedProperty("min",
        min == null ? null : DateTimeField.toDateTime(min.truncatedTo(ChronoUnit.SECONDS)));
    return this;
  }

  /**
   * Get the earliest date and time to accept.
   *
   * @return the earliest date and time to accept
   */
  @Override
  public LocalDateTime getMin() {
    return min;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public DateTimeField setText(String text) {
    if (text != null && !text.isEmpty() && !DateTimeField.isValidDateTime(text)) {
      throw new IllegalArgumentException(
          "Text must be a valid date and time in yyyy-MM-ddTHH:mm:ss format");
    }

    if (min != null && text != null && compareDateTime(LocalDateTime.parse(text), min) < 0) {
      throw new IllegalArgumentException(
          "Date and time must be later than or equal to the minimum date and time");
    }

    if (max != null && text != null && compareDateTime(LocalDateTime.parse(text), max) > 0) {
      throw new IllegalArgumentException(
          "Date and time must be earlier than or equal to the maximum date and time");
    }

    super.setText(text);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public DateTimeField setValue(LocalDateTime value) {
    setText(value == null ? null : DateTimeField.toDateTime(value.truncatedTo(ChronoUnit.SECONDS)));
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public LocalDateTime getValue() {
    String text = getText();
    return text == null || text.isEmpty() ? null
        : LocalDateTime.parse(text).truncatedTo(ChronoUnit.SECONDS);
  }

  /**
   * Convert a date and time string in yyyy-MM-ddTHH:mm:ss format to a LocalDateTime.
   *
   * @param dateTimeAsString the date and time string in yyyy-MM-ddTHH:mm:ss format
   * @return the LocalDateTime
   */
  public static LocalDateTime fromDateTime(String dateTimeAsString) {
    return LocalDateTime.parse(dateTimeAsString);
  }

  /**
   * Convert a LocalDateTime to a date and time string in yyyy-MM-ddTHH:mm:ss format.
   *
   * @param dateTime the LocalDateTime
   * @return the date and time string in yyyy-MM-ddTHH:mm:ss format
   */
  public static String toDateTime(LocalDateTime dateTime) {
    return dateTime.toString();
  }

  /**
   * Check if the given string is a valid yyyy-MM-ddTHH:mm:ss date and time.
   *
   * @param dateTimeAsString the date and time string
   * @return true if the string is a valid date and time, false otherwise
   */
  public static boolean isValidDateTime(String dateTimeAsString) {
    try {
      LocalDateTime.parse(dateTimeAsString);
      return true;
    } catch (Exception e) {
      return false;
    }
  }

  private int compareDateTime(LocalDateTime dateTime1, LocalDateTime dateTime2) {
    LocalDateTime truncatedDateTime1 = dateTime1.truncatedTo(ChronoUnit.SECONDS);
    LocalDateTime truncatedDateTime2 = dateTime2.truncatedTo(ChronoUnit.SECONDS);
    return truncatedDateTime1.compareTo(truncatedDateTime2);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected LocalDateTime convertValue(String value) {
    return DateTimeField.fromDateTime(value);
  }
}
