package com.webforj.data.binding.event;

import com.webforj.data.binding.Binding;
import com.webforj.data.concern.ValueAware;
import com.webforj.data.validation.server.ValidationResult;
import java.util.EventObject;

/**
 * Represents an event that is fired when a binding is validated.
 *
 * @param <C> The type of the component which the binding is bound to.
 * @param <B> The type of the bean which the binding is bound to.
 * @param <BV> The type of the value of the binding.
 *
 * @since 24.01
 * @author Hyyan Abo Fakher
 */
public class BindingValidateEvent<C extends ValueAware<C, CV>, CV, B, BV> extends EventObject {
  private final transient Binding<C, CV, B, BV> binding;
  private final transient ValidationResult validationResult;
  private final transient CV value;

  /**
   * Creates a new instance of {@code BindingValidateEvent}.
   *
   * @param source The field binding.
   * @param validationResult The validation result.
   * @param value The value of the binding.
   */
  public BindingValidateEvent(Binding<C, CV, B, BV> source, ValidationResult validationResult,
      CV value) {
    super(source);
    this.binding = source;
    this.validationResult = validationResult;
    this.value = value;
  }

  /**
   * Gets the field binding.
   *
   * @return The field binding.
   */
  public Binding<C, CV, B, BV> getBinding() {
    return binding;
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
   * Gets the value of the field binding.
   *
   * @return The value of the field binding.
   */
  public CV getValue() {
    return value;
  }
}
