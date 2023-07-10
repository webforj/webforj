package org.dwcj.component.field;

import java.time.LocalTime;
import java.time.temporal.ChronoUnit;
import org.dwcj.annotation.ExcludeFromJacocoGeneratedReport;
import org.dwcj.component.HasMax;
import org.dwcj.component.HasMin;
import org.dwcj.component.HighlightableOnFocus;

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
public final class TimeField extends AbstractField<TimeField, LocalTime> implements
    HasMin<TimeField, LocalTime>, HasMax<TimeField, LocalTime>, HighlightableOnFocus<TimeField> {

  private LocalTime min = null;
  private LocalTime max = null;

  /**
   * Constructs a new TimeField with the given label, initial value, and option to accept seconds.
   *
   * @param label the label for the field
   * @param time the initial value for the field
   */
  public TimeField(String label, LocalTime time) {
    super();
    setUnrestrictedProperty("type", "time");
    setLabel(label);
    setValue(time);
  }

  /**
   * Constructs a new TimeField with the given label.
   *
   * @param label the label for the field
   */
  public TimeField(String label) {
    this(label, null);
  }

  /**
   * Constructs a new TimeField with tye given initial value.
   *
   * @param time the initial value for the field
   */
  public TimeField(LocalTime time) {
    this("", time);
  }

  /**
   * Constructs a new TimeField with default settings.
   */
  public TimeField() {
    this("", null);
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
    setUnrestrictedProperty("max",
        max == null ? null : TimeField.toTime(max.truncatedTo(ChronoUnit.SECONDS)));
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
    setUnrestrictedProperty("min",
        min == null ? null : TimeField.toTime(min.truncatedTo(ChronoUnit.SECONDS)));
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

    super.setText(text);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public TimeField setValue(LocalTime value) {
    setText(value == null ? null : TimeField.toTime(value.truncatedTo(ChronoUnit.SECONDS)));
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
  public TimeField setHighlightOnFocus(Behavior highlight) {
    setComponentHighlightOnFocus(highlight);
    return getSelf();
  }

  /**
   * Convert a time string in HH:mm:ss format to a LocalTime.
   *
   * @param timeAsString the time string in HH:mm:ss format
   * @return the LocalTime
   */
  public static LocalTime fromTime(String timeAsString) {
    return LocalTime.parse(timeAsString);
  }

  /**
   * Convert a LocalTime to a time string in HH:mm:ss format.
   *
   * @param time the LocalTime
   * @return the time string in HH:mm:ss format
   */
  public static String toTime(LocalTime time) {
    return time.toString();
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

  private int compareTime(LocalTime time1, LocalTime time2) {
    int hourComparison = Integer.compare(time1.getHour(), time2.getHour());
    if (hourComparison != 0) {
      return hourComparison;
    }

    int minuteComparison = Integer.compare(time1.getMinute(), time2.getMinute());
    if (minuteComparison != 0) {
      return minuteComparison;
    }

    return Integer.compare(time1.getSecond(), time2.getSecond());
  }
}
