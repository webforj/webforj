package com.webforj.data.validation.server.validator;

import com.webforj.data.validation.server.ValidationResult;
import java.util.function.Predicate;
import java.util.function.Supplier;

/**
 * Represents a validator that can be used to validate a value of type V on the server side.
 *
 * @param <V> the type of the value that is being validated.
 *
 * @since 24.01
 * @author Hyyan Abo Fakher
 */
@FunctionalInterface
public interface Validator<V> {

  /**
   * Validates the given value.
   *
   * @param value the value to validate.
   * @return the validation result.
   */
  ValidationResult validate(V value);

  /**
   * Creates a validator from a predicate.
   *
   * @param <V> the type of the value that is being validated.
   * @param predicate the predicate to use for validation.
   *
   * @return a validator that uses the predicate to validate the value.
   */
  public static <V> Validator<V> of(Predicate<V> predicate, String message) {
    return of(predicate, () -> message);
  }

  /**
   * Creates a validator from a predicate with a message supplier. The supplier is invoked each time
   * validation fails, allowing the message to be resolved dynamically.
   *
   * @param <V> the type of the value that is being validated.
   * @param predicate the predicate to use for validation.
   * @param messageSupplier the supplier providing the failure message.
   *
   * @return a validator that uses the predicate to validate the value.
   *
   * @since 25.12
   */
  public static <V> Validator<V> of(Predicate<V> predicate, Supplier<String> messageSupplier) {
    return value -> predicate.test(value) ? ValidationResult.valid()
        : ValidationResult.invalid(messageSupplier.get());
  }

  /**
   * Creates a validator that uses another validator to validate the value.
   *
   * @param <V> the type of the value that is being validated.
   * @param validator the validator to use.
   * @param message the message to use in case of validation failure.
   *
   * @return a validator that uses the given validator to validate the value.
   */
  public static <V> Validator<V> from(Validator<V> validator, String message) {
    return from(validator, () -> message);
  }

  /**
   * Creates a validator that uses another validator to validate the value with a message supplier.
   * The supplier is invoked each time validation fails, allowing the message to be resolved
   * dynamically.
   *
   * @param <V> the type of the value that is being validated.
   * @param validator the validator to use.
   * @param messageSupplier the supplier providing the failure message.
   *
   * @return a validator that uses the given validator to validate the value.
   *
   * @since 25.12
   */
  public static <V> Validator<V> from(Validator<V> validator, Supplier<String> messageSupplier) {
    return value -> {
      ValidationResult result = validator.validate(value);
      if (result.isValid()) {
        return ValidationResult.valid();
      } else {
        String message = messageSupplier.get();
        if (message == null || message.isEmpty()) {
          return ValidationResult.invalid(result.getMessages());
        } else {
          return ValidationResult.invalid(message);
        }
      }
    };
  }
}
