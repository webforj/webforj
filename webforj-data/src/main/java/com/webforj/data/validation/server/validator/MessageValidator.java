package com.webforj.data.validation.server.validator;

import com.webforj.data.LocaleAware;
import com.webforj.data.ZoneAware;
import com.webforj.data.validation.server.ValidationResult;
import java.time.ZoneId;
import java.util.Locale;
import java.util.function.Supplier;

/**
 * Validator that reports the failure of another validator with its own message.
 *
 * <p>
 * The locale and the time zone are passed to the other validator when it is {@link LocaleAware} or
 * {@link ZoneAware}.
 * </p>
 *
 * @param <V> the type of the value that is being validated.
 *
 * @author Hyyan Abo Fakher
 * @since 26.10
 */
final class MessageValidator<V>
    implements Validator<V>, LocaleAware<MessageValidator<V>>, ZoneAware<MessageValidator<V>> {
  private final Validator<V> validator;
  private final Supplier<String> messageSupplier;

  MessageValidator(Validator<V> validator, Supplier<String> messageSupplier) {
    this.validator = validator;
    this.messageSupplier = messageSupplier;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ValidationResult validate(V value) {
    ValidationResult result = validator.validate(value);
    if (result.isValid()) {
      return ValidationResult.valid();
    }

    String message = messageSupplier.get();
    if (message == null || message.isEmpty()) {
      return ValidationResult.invalid(result.getMessages());
    }

    return ValidationResult.invalid(message);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public MessageValidator<V> setLocale(Locale locale) {
    if (validator instanceof LocaleAware<?> localeAware) {
      localeAware.setLocale(locale);
    }

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Locale getLocale() {
    return validator instanceof LocaleAware<?> localeAware ? localeAware.getLocale() : null;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public MessageValidator<V> setZone(ZoneId zone) {
    if (validator instanceof ZoneAware<?> zoneAware) {
      zoneAware.setZone(zone);
    }

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ZoneId getZone() {
    return validator instanceof ZoneAware<?> zoneAware ? zoneAware.getZone() : null;
  }
}
