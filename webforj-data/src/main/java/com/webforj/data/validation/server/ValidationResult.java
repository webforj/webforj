package com.webforj.data.validation.server;

import java.util.Collections;
import java.util.List;

/**
 * Represents a validation result.
 *
 * @since 24.01
 * @author Hyyan Abo Fakher
 */
public class ValidationResult {
  private final boolean isValid;
  private final List<String> messages;

  private ValidationResult(boolean isValid, List<String> messages) {
    this.isValid = isValid;
    this.messages = messages;
  }

  /**
   * Creates a valid result.
   *
   * @return a valid result.
   */
  public static ValidationResult valid() {
    return new ValidationResult(true, Collections.emptyList());
  }

  /**
   * Creates an invalid result.
   *
   * @param messages the validation messages.
   * @return an invalid result.
   */
  public static ValidationResult invalid(List<String> messages) {
    return new ValidationResult(false, messages);
  }

  /**
   * Creates an invalid result.
   *
   * @param message the validation message.
   * @return an invalid result.
   */
  public static ValidationResult invalid(String message) {
    return new ValidationResult(false, Collections.singletonList(message));
  }

  /**
   * Checks if the result is valid.
   *
   * @return true if the result is valid, false otherwise.
   */
  public boolean isValid() {
    return isValid;
  }

  /**
   * Retrieves the validation messages.
   *
   * @return the validation messages.
   */
  public List<String> getMessages() {
    return messages;
  }
}
