package com.webforj.data.transformation.transformer;

import com.webforj.data.ZoneAware;
import com.webforj.data.transformation.TransformationException;
import java.time.DateTimeException;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Objects;

/**
 * Represents a transformer between a LocalDateTime component value and a ZonedDateTime model value
 * in a time zone.
 *
 * <p>
 * A model value outside the LocalDateTime range in the time zone fails the transformation. A null
 * value stays null in both directions.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.10
 */
public final class LocalDateTimeZonedDateTimeTransformer implements
    Transformer<LocalDateTime, ZonedDateTime>, ZoneAware<LocalDateTimeZonedDateTimeTransformer> {
  private static final String MESSAGE = "The value is outside the supported date and time range.";
  private ZoneId zone = ZoneId.systemDefault();

  /**
   * {@inheritDoc}
   *
   * <p>
   * The time zone defaults to the system default time zone of the server. A binding context passes
   * its own time zone when the binding is added and on every change.
   * </p>
   *
   * @throws NullPointerException if the zone is null
   */
  @Override
  public LocalDateTimeZonedDateTimeTransformer setZone(ZoneId zone) {
    this.zone = Objects.requireNonNull(zone, "zone cannot be null");
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ZoneId getZone() {
    return zone;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ZonedDateTime transformToModel(LocalDateTime viewValue) {
    return viewValue == null ? null : viewValue.atZone(zone);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public LocalDateTime transformToComponent(ZonedDateTime modelValue) {
    if (modelValue == null) {
      return null;
    }

    try {
      return modelValue.withZoneSameInstant(zone).toLocalDateTime();
    } catch (DateTimeException e) {
      throw new TransformationException(MESSAGE, e);
    }
  }
}
