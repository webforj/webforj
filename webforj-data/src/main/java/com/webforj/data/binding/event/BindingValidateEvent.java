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
 * @param <BeanValueT> The type of the value of the binding.
 *
 * @since 24.01
 * @author Hyyan Abo Fakher
 */
public class BindingValidateEvent<C extends ValueAware<C, ComponentValueT>, ComponentValueT, B, BeanValueT> extends EventObject {
  private final transient Binding<C, ComponentValueT, B, BeanValueT> binding;
  private final transient ValidationResult validationResult;
  private final transient ComponentValueT value;

  /**
   * Creates a new instance of {@code BindingValidateEvent}.
   *
   * @param source The field binding.
   * @param validationResult The validation result.
   * @param value The value of the binding.
   */
  public BindingValidateEvent(Binding<C, ComponentValueT, B, BeanValueT> source, ValidationResult validationResult,
      ComponentValueT value) {
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
  public Binding<C, ComponentValueT, B, BeanValueT> getBinding() {
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
  public ComponentValueT getValue() {
    return value;
  }
}
