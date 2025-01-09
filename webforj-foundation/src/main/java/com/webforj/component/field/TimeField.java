package com.webforj.component.field;

import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import com.webforj.concern.HasMax;
import com.webforj.concern.HasMin;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventListener;

/**
 * The TimeField provides a user interface component that designed to let the user easily enter a
 * time (hours and minutes, and optionally seconds).
 *
 * <p>
 * The control's user interface varies from browser to browser. The value of the time field is
 * always in 24-hour format that includes leading zeros: HH:mm, regardless of the field format,
 * which is likely to be selected based on the user's locale (or by the user agent). If the time
 * includes seconds, the format is always HH:mm:ss
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
public final class TimeField extends DwcFieldInitializer<TimeField, LocalTime>
    implements HasMin<TimeField, LocalTime>, HasMax<TimeField, LocalTime> {

  private LocalTime min = null;
  private LocalTime max = null;

  /**
   * Constructs a new time field with a label, value, and a value change listener.
   *
   * @param label the label of the field
   * @param value the value of the field
   * @param listener the value change listener
   */
  public TimeField(String label, LocalTime value,
      EventListener<ValueChangeEvent<LocalTime>> listener) {
    super(label, value, listener);
    postInit();
  }

  /**
   * Constructs a new time field with a label and a value change listener.
   *
   * @param label the label of the field
   * @param listener the value change listener
   */
  public TimeField(String label, EventListener<ValueChangeEvent<LocalTime>> listener) {
    super(label, listener);
    postInit();
  }

  /**
   * Constructs a new time field with a value change listener.
   *
   * @param listener the value change listener
   */
  public TimeField(EventListener<ValueChangeEvent<LocalTime>> listener) {
    super(listener);
    postInit();
  }

  /**
   * Constructs a new time field with a label.
   *
   * @param label the label of the field
   */
  public TimeField(String label) {
    super(label);
    postInit();
  }

  /**
   * Construct a new time field with the given value.
   *
   * @param value the value for the field
   */
  public TimeField(LocalTime value) {
    super();
    setValue(value);
    postInit();
  }

  /**
   * Construct a new time field with the given label and value.
   *
   * @param label the label for the field
   * @param value the value for the field
   */
  public TimeField(String label, LocalTime value) {
    super(label);
    setValue(value);
    postInit();
  }

  /**
   * Constructs a new time field.
   */
  public TimeField() {
    super();
    postInit();
  }

  private void postInit() {
    setUnrestrictedProperty("type", "time");
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public TimeField setMax(LocalTime max) {
    LocalTime currentValue = getValue();

    if (max != null && currentValue != null && compareTime(max, currentValue) < 0) {
      throw new IllegalArgumentException(
          "Maximum time must be later than or equal to the current value");
    }

    if (min != null && max != null && max.isBefore(min)) {
      throw new IllegalArgumentException(
          "Maximum time must be later than or equal to the minimum time");
    }

    this.max = max;
    setUnrestrictedProperty("max", max == null ? null : TimeField.toTime(max));
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public LocalTime getMax() {
    return max;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public TimeField setMin(LocalTime min) {
    LocalTime currentValue = getValue();

    if (min != null && currentValue != null && compareTime(min, currentValue) > 0) {
      throw new IllegalArgumentException(
          "Minimum time must be earlier than or equal to the current value");
    }

    if (max != null && min != null && max.isBefore(min)) {
      throw new IllegalArgumentException(
          "Minimum time must be earlier than or equal to the maximum time");
    }

    this.min = min;
    setUnrestrictedProperty("min", min == null ? null : TimeField.toTime(min));
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public LocalTime getMin() {
    return min;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public TimeField setText(String text) {
    if (text != null && !text.isEmpty() && !TimeField.isValidTime(text)) {
      throw new IllegalArgumentException("Text must be a valid time in HH:mm:ss format");
    }

    if (min != null && text != null && compareTime(min, LocalTime.parse(text)) > 0) {
      throw new IllegalArgumentException("Time must be later than or equal to the minimum time");
    }

    if (max != null && text != null && compareTime(max, LocalTime.parse(text)) < 0) {
      throw new IllegalArgumentException("Time must be earlier than or equal to the maximum time");
    }

    super.setText(
        text != null && !text.isEmpty() ? TimeField.toTime(TimeField.fromTime(text)) : "");
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public TimeField setValue(LocalTime value) {
    setText(value == null ? null : TimeField.toTime(value));
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public LocalTime getValue() {
    String text = getText();
    return text == null || text.isEmpty() ? null : TimeField.fromTime(text);
  }

  /**
   * Convert a time string in HH:mm:ss format to a LocalTime.
   *
   * @param timeAsString the time string in HH:mm:ss format
   * @return the LocalTime
   */
  public static LocalTime fromTime(String timeAsString) {
    return LocalTime.parse(timeAsString).truncatedTo(ChronoUnit.MINUTES);
  }

  /**
   * Convert a LocalTime to a time string in HH:mm format.
   *
   * @param time the LocalTime
   * @return the time string in HH:mm format
   */
  public static String toTime(LocalTime time) {
    return time.truncatedTo(ChronoUnit.MINUTES).format(DateTimeFormatter.ofPattern("HH:mm"));
  }

  /**
   * Check if the given string is a valid HH:mm:ss time.
   *
   * @param timeAsString the time string
   * @return true if the string is a valid time, false otherwise
   */
  public static boolean isValidTime(String timeAsString) {
    try {
      LocalTime.parse(timeAsString);
      return true;
    } catch (Exception e) {
      return false;
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected LocalTime convertValue(String value) {
    return TimeField.fromTime(value);
  }

  private int compareTime(LocalTime time1, LocalTime time2) {
    return time1.compareTo(time2);
  }
}
