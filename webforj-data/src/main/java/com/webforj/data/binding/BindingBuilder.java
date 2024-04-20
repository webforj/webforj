package com.webforj.data.binding;

import com.webforj.data.concern.ValueAware;
import com.webforj.data.transformation.transformer.Transformer;
import com.webforj.data.validation.InvalidAware;
import com.webforj.data.validation.server.validator.Validator;
import java.util.List;
import java.util.Locale;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.function.Predicate;

/**
 * A builder for creating bindings.
 *
 * @param <C> the type of the UI component implementing {@link ValueAware}, capable of displaying
 *        and updating a value of type V.
 * @param <B> the type of the bean from which the value is being synchronized.
 * @param <BV> the type of the value that is being bound.
 *
 * @since 24.01
 * @author Hyyan Abo Fakher
 */
public interface BindingBuilder<C extends ValueAware<C, CV>, CV, B, BV> {

  /**
   * Sets the bean property getter function.
   *
   * @param getter the getter function.
   * @return this binding builder.
   */
  public BindingBuilder<C, CV, B, BV> useGetter(Function<B, BV> getter);

  /**
   * Sets the bean property setter function.
   *
   * @param setter the setter function.
   * @return this binding builder.
   */
  public BindingBuilder<C, CV, B, BV> useSetter(BiConsumer<B, BV> setter);

  /**
   * Sets the bean property getter and setter functions.
   *
   * @param setter the setter function.
   * @param getter the getter function.
   *
   * @return this binding builder.
   */
  public default BindingBuilder<C, CV, B, BV> useMutator(BiConsumer<B, BV> setter,
      Function<B, BV> getter) {
    return useGetter(getter).useSetter(setter);
  }

  /**
   * Sets the transformer to use for transforming the value between the component and the bean.
   *
   * @param transformer the transformer.
   * @param message the message to be used in case of transformation failure.
   *
   * @return this binding builder.
   */
  public BindingBuilder<C, CV, B, BV> useTransformer(Transformer<CV, BV> transformer,
      String message);

  /**
   * Sets the transformer to use for transforming the value between the component and the bean.
   *
   * @param transformer the transformer.
   *
   * @return this binding builder.
   */
  public default BindingBuilder<C, CV, B, BV> useTransformer(Transformer<CV, BV> transformer) {
    return useTransformer(transformer, null);
  }

  /**
   * Sets the read-only state of the binding.
   *
   * <p>
   * When the binding is set to read-only, the component will not be updated when the value of the
   * component changes. This is useful when the component is used to display the value but should
   * not be updated.
   * </p>
   *
   * <p>
   * If the component implements {@link ReadOnlyAware}, the read-only state will be set on the
   * component.
   * </p>
   *
   * @param readOnly The read-only state.
   */
  public BindingBuilder<C, CV, B, BV> readOnly(boolean readOnly);

  /**
   * Sets the read-only state of the binding.
   *
   * <p>
   * When the binding is set to read-only, the component will not be updated when the value of the
   * component changes. This is useful when the component is used to display the value but should
   * not be updated.
   * </p>
   *
   * <p>
   * If the component implements {@link ReadOnlyAware}, the read-only state will be set on the
   * component.
   * </p>
   */
  public default BindingBuilder<C, CV, B, BV> readOnly() {
    return readOnly(true);
  }

  /**
   * Sets the required state of the binding.
   *
   * <p>
   * When the binding is set to required, the component will be marked as required if the component
   * supports the required state using the {@link RequiredAware} interface. The binding itself will
   * not enforce the required state, but it will set the required state on the component if the
   * component supports it, providing a validation is still required.
   * </p>
   *
   * @param required The required state.
   */
  public BindingBuilder<C, CV, B, BV> required(boolean required);

  /**
   * Sets the required state of the binding.
   *
   * <p>
   * When the binding is set to required, the component will be marked as required if the component
   * supports the required state using the {@link RequiredAware} interface. The binding itself will
   * not enforce the required state, but it will set the required state on the component if the
   * component supports it, providing a validation is still required.
   * </p>
   */
  public default BindingBuilder<C, CV, B, BV> required() {
    return required(true);
  }

  /**
   * Adds a validator to the binding.
   *
   * @param validator the validator.
   * @return this binding builder.
   */
  public BindingBuilder<C, CV, B, BV> useValidator(Validator<BV> validator);

  /**
   * Adds a validator to the binding.
   *
   * @param validator the validator.
   * @param message the message to be used in case of validation failure.
   *
   * @return this binding builder.
   */
  public BindingBuilder<C, CV, B, BV> useValidator(Validator<BV> validator, String message);

  /**
   * Adds a simple boolean validator with a corresponding message.
   *
   * @param validator A function that takes a value of type V and returns a Boolean indicating
   *        validity.
   * @param message The message to be used in case of validation failure.
   * @return this binding builder.
   */
  public BindingBuilder<C, CV, B, BV> useValidator(Predicate<BV> validator, String message);

  /**
   * Adds a list of validators to the binding.
   *
   * @param validators the list of validators.
   * @param messages the list of messages corresponding to the validators.
   *
   * @return this binding builder.
   */
  public BindingBuilder<C, CV, B, BV> useValidatorsList(List<Validator<BV>> validators,
      List<String> messages);

  /**
   * Adds a list of validators to the binding.
   *
   * @param validators the list of validators.
   * @return this binding builder.
   */
  public default BindingBuilder<C, CV, B, BV> useValidatorsList(List<Validator<BV>> validators) {
    return useValidatorsList(validators, null);
  }

  /**
   * Will process the value through the Jakarta Validation API using the given locale.
   *
   * @param locale the locale to use
   * @return this binding builder.
   */
  public BindingBuilder<C, CV, B, BV> useJakartaValidator(Locale locale);

  /**
   * Will process the value through the Jakarta Validation API using the default locale.
   *
   * @return this binding builder.
   */
  public default BindingBuilder<C, CV, B, BV> useJakartaValidator() {
    return useJakartaValidator(Locale.getDefault());
  }

  /**
   * Sets the validation reporter.
   *
   * @param reporter the validation reporter.
   * @return this binding builder.
   */
  public BindingBuilder<C, CV, B, BV> useReporter(BindingReporter<C, CV, B, BV> reporter);

  /**
   * Sets the default validation reporter.
   *
   * <p>
   * <b>Note:</b> the default reporter sets the component as invalid and displays the validation
   * messages as a single string using the {@link InvalidAware} interface. If the component does not
   * implement the {@link InvalidAware} interface, the default reporter does nothing.
   * </p>
   *
   * @param formatter the function to format the validation messages.
   * @return this binding builder.
   */
  public BindingBuilder<C, CV, B, BV> useDefaultReporter(Function<List<String>, String> formatter);

  /**
   * Sets the default validation reporter.
   *
   * <p>
   * <b>Note:</b> the default reporter sets the component as invalid and displays the validation
   * messages as a single string using the {@link InvalidAware} interface. If the component does not
   * implement the {@link InvalidAware} interface, the default reporter does nothing.
   * </p>
   *
   * @see #useDefaultReporter(Function)
   *
   * @return this binding builder.
   */
  public default BindingBuilder<C, CV, B, BV> useDefaultReporter() {
    return useDefaultReporter(null);
  }

  /**
   * Monitors the component for changes and validates the value on each change based on the
   * validators added to the binding.
   *
   * <p>
   * By default, validation is only performed when the value is written to the bean. This method
   * enables validation on each change to the component value providing immediate feedback to the
   * user.
   * </p>
   *
   * @param autoValidate whether to validate on each change.
   * @return this binding builder.
   */
  public BindingBuilder<C, CV, B, BV> autoValidate(boolean autoValidate);

  /**
   * Monitors the component for changes and validates the value on each change based on the
   * validators added to the binding.
   *
   * <p>
   * By default, validation is only performed when the value is written to the bean. This method
   * enables validation on each change to the component value providing immediate feedback to the
   * user.
   * </p>
   *
   * @return this binding builder.
   */
  public default BindingBuilder<C, CV, B, BV> autoValidate() {
    return autoValidate(true);
  }

  /**
   * Adds the binding to the binding context.
   *
   * @return the binding context.
   */
  public BindingContext<B> add();

  /**
   * Removes the binding from the binding context.
   */
  public void remove();
}
