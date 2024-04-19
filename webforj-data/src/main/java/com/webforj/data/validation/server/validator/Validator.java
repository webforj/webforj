package com.webforj.data.validation.server.validator;

import com.webforj.data.validation.server.ValidationResult;
import java.util.function.Predicate;

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
    return value -> predicate.test(value) ? ValidationResult.valid()
        : ValidationResult.invalid(message);
  }

  /**
   * Creates a validator that uses another validator to validate the value.
   *
   * @param <V> the type of the value that is being validated.
   * @param validator the validator to use.
   *
   * @return a validator that uses the given validator to validate the value.
   */
  public static <V> Validator<V> of(Validator<V> validator, String message) {
    return value -> {
      ValidationResult result = validator.validate(value);
      if (result.isValid()) {
        return ValidationResult.valid();
      } else {
        if (message == null || message.isEmpty()) {
          return ValidationResult.invalid(result.getMessages());
        } else {
          return ValidationResult.invalid(message);
        }
      }
    };
  }
}
