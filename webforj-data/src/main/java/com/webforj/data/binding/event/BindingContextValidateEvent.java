package com.webforj.data.binding.event;

import com.webforj.data.binding.BindingContext;
import com.webforj.data.validation.server.ValidationResult;
import java.util.EventObject;

/**
 * Represents an event that is fired when the bindings in the binding context are validated.
 *
 * @param <B> The type of the bean which the binding context is bound to.
 *
 * @author Hyyan Abo Fakher
 * @since 24.01
 */
public class BindingContextValidateEvent<B> extends EventObject {
  private final transient BindingContext<B> context;
  private final transient ValidationResult validationResult;

  /**
   * Creates a new instance of {@code BindingContextValidateEvent}.
   *
   * @param source The binding context.
   * @param validationResult The validation result.
   */
  public BindingContextValidateEvent(BindingContext<B> source, ValidationResult validationResult) {
    super(source);
    this.context = source;
    this.validationResult = validationResult;
  }

  /**
   * Gets the validation result.
   *
   * @return The validation result.
   */
  public ValidationResult getValidationResult() {
    return validationResult;
  }

  /**
   * Checks if the validation result is valid.
   *
   * @return true if the validation result is valid, false otherwise.
   */
  public boolean isValid() {
    return validationResult.isValid();
  }

  /**
   * Gets the binding context.
   *
   * @return The binding context.
   */
  public BindingContext<B> getBindingContext() {
    return context;
  }
}
