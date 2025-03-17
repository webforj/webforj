package com.webforj.data.binding;

import com.webforj.data.BeanAware;
import com.webforj.data.BeanUtils;
import com.webforj.data.MutatorException;
import com.webforj.data.PropertyNameAware;
import com.webforj.data.binding.event.BindingValidateEvent;
import com.webforj.data.concern.ReadOnlyAware;
import com.webforj.data.concern.RequiredAware;
import com.webforj.data.concern.ValueAware;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.data.transformation.TransformationException;
import com.webforj.data.transformation.transformer.Transformer;
import com.webforj.data.validation.client.AutoClientValidation;
import com.webforj.data.validation.client.AutoClientValidationOnLoad;
import com.webforj.data.validation.server.ValidationResult;
import com.webforj.data.validation.server.validator.Validator;
import com.webforj.dispatcher.EventDispatcher;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import java.beans.IntrospectionException;
import java.beans.PropertyDescriptor;
import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.function.BiConsumer;
import java.util.function.Function;

/**
 * Represents a binding between a UI component and a property of a Java Bean.
 *
 * <p>
 * The class is designed to create a dynamic link between a UI component and a property of a Java
 * Bean. The primary goal of this class is to facilitate the seamless flow of data between the UI
 * layer and the data model, ensuring that any changes in the UI are reflected in the model and vice
 * versa. This bidirectional binding enables developers to build more responsive and interactive
 * applications.
 * </p>
 *
 * <p>
 * The binding is established through a combination of getter and setter functions, encapsulating
 * the access and modification of the bean's property. Additionally, supports validation, allowing
 * for the integration of custom validation logic to ensure data integrity before updating the
 * model.
 * </p>
 *
 * @param <C> the type of the UI component implementing {@link ValueAware}, capable of displaying
 *        and updating a value of type V.
 * @param <B> the type of the bean from which the value is being synchronized.
 * @param <BV> the type of the value that is being bound.
 *
 * @since 24.01
 * @author Hyyan Abo Fakher
 */
public class Binding<C extends ValueAware<C, CV>, CV, B, BV> {
  private final EventDispatcher dispatcher = new EventDispatcher();
  private final Class<B> beanClass;
  private final C component;
  private Function<B, BV> getter;
  private BiConsumer<B, BV> setter;
  private String property;
  private Transformer<CV, BV> transformer;
  private String transformerMessage;
  private final List<Validator<BV>> validators = new ArrayList<>();
  private BindingReporter<C, CV, B, BV> validateReporter;
  private boolean autoValidate = false;
  private ListenerRegistration<ValueChangeEvent<CV>> valueChangeListener;
  private boolean readOnly = false;
  private boolean required = false;
  private boolean autoWrite = false;
  private B autoWriteBean = null;
  private CV cachedValue;
  private boolean isValueCached = false;
  private Class<?> lastBeanClassForGetter;
  private Function<B, BV> cachedGetter;
  private Class<?> lastBeanClassForSetter;
  private BiConsumer<B, BV> cachedSetter;

  /**
   * Constructs a new FieldBinding.
   *
   * @param beanClass The class of the bean
   * @param component The UI component that will display the value.
   * @param property The property of the bean that the binding is associated with.
   */
  Binding(C component, Class<B> beanClass, String property) {
    this.component = Objects.requireNonNull(component, "component cannot be null");
    this.beanClass = Objects.requireNonNull(beanClass, "beanClass cannot be null");
    this.property = property;

    autoDetectedRequired();
  }

  /**
   * Retrieves the component of the binding.
   *
   * @return the component.
   */
  public C getComponent() {
    return component;
  }

  /**
   * Retrieves the property that the binding is associated with.
   *
   * @return the property.
   */
  public String getProperty() {
    return property;
  }

  /**
   * Sets the getter of the java bean.
   *
   * <p>
   * The getter is used to read the value of the property from the bean instance. If the getter is
   * not set, the binding will attempt to resolve the getter using the property name. Use
   * {@link #setGetter(Function)} to set a custom getter function in case the property name does not
   * match the getter method name or if a transformation is required to obtain the value.
   * </p>
   *
   * @param getter the getter.
   * @return this binding itself.
   */
  public Binding<C, CV, B, BV> setGetter(Function<B, BV> getter) {
    this.getter = getter;
    return this;
  }

  /**
   * Retrieves the getter of the java bean.
   *
   * <p>
   * If the getter is not set explicitly, this method will return an empty optional and wont attempt
   * to resolve the getter using the property name.
   * </p>
   *
   * @return the getter.
   * @see #setGetter(Function)
   */
  public Optional<Function<B, BV>> getGetter() {
    return Optional.ofNullable(getter);
  }

  /**
   * Sets the setter of the java bean.
   *
   * <p>
   * The setter is used to update the value of the property in the bean instance. If the setter is
   * not set, the binding will attempt to resolve the setter using the property name. Use
   * {@link #setSetter(BiConsumer)} to set a custom setter function in case the property name does
   * not match the setter method name or if a transformation is required to update the value.
   * </p>
   *
   * @param setter the setter.
   * @return this binding itself.
   */
  public Binding<C, CV, B, BV> setSetter(BiConsumer<B, BV> setter) {
    this.setter = setter;
    return this;
  }

  /**
   * Retrieves the setter of the java bean.
   *
   * <p>
   * If the setter is not set explicitly, this method will return an empty optional and wont attempt
   * to resolve the setter using the property name.
   * </p>
   *
   * @return the setter.
   */
  public Optional<BiConsumer<B, BV>> getSetter() {
    return Optional.ofNullable(setter);
  }

  /**
   * Sets the transformer to the binding.
   *
   * <p>
   * The transformer is used to transform the value between the component and the bean. The
   * transformer is useful when the component value and the bean value are of different types and
   * require conversion.
   * </p>
   *
   * @param transformer The transformer to set.
   * @param message The message to display when the transformation fails.
   *
   * @return this binding itself.
   */
  public Binding<C, CV, B, BV> setTransformer(Transformer<CV, BV> transformer, String message) {
    this.transformer = transformer;
    this.transformerMessage = message;
    return this;
  }

  /**
   * Sets the transformer to the binding.
   *
   * <p>
   * The transformer is used to transform the value between the component and the bean. The
   * transformer is useful when the component value and the bean value are of different types and
   * require conversion.
   * </p>
   *
   * @param transformer The transformer to set.
   *
   * @return this binding itself.
   */
  public Binding<C, CV, B, BV> setTransformer(Transformer<CV, BV> transformer) {
    return setTransformer(transformer, null);
  }

  /**
   * Retrieves the transformer of the binding.
   *
   * @return the transformer.
   */
  public Optional<Transformer<CV, BV>> getTransformer() {
    return Optional.ofNullable(transformer);
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
   * @return this binding itself.
   */
  @SuppressWarnings("squid:S3740")
  public Binding<C, CV, B, BV> setReadOnly(boolean readOnly) {
    this.readOnly = readOnly;
    if (component instanceof ReadOnlyAware readonlyAware) {
      readonlyAware.setReadOnly(readOnly);
    }

    return this;
  }

  /**
   * Retrieves the read-only state of the binding.
   *
   * @return the read-only state.
   */
  public boolean isReadOnly() {
    return readOnly;
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
   * @return this binding itself.
   */
  @SuppressWarnings("squid:S3740")
  public Binding<C, CV, B, BV> setRequired(boolean required) {
    this.required = required;
    updateClientAutoValidation(!required);

    if (component instanceof RequiredAware requiredAware) {
      requiredAware.setRequired(required);
    }

    return this;
  }

  /**
   * Retrieves the required state of the binding.
   *
   * @return the required state.
   */
  public boolean isRequired() {
    return required;
  }

  /**
   * Adds a validator to the binding.
   *
   * <p>
   * The validator is used to validate the value of the component before updating the bean.
   * Implementations of the {@link Validator} interface can be used to add custom validation logic.
   * If the value is invalid, the validation result will be reported using the validation reporter
   * and the bean will not be updated.
   * </p>
   *
   * @param validator The validator to add.
   * @return this binding itself.
   */
  public Binding<C, CV, B, BV> addValidator(Validator<BV> validator) {
    validators.add(validator);
    return this;
  }

  /**
   * Sets the list of validators to the binding.
   *
   * <p>
   * The method will clear the existing list of validators and add the new list of validators.
   * </p>
   *
   * @param validators The list of validators to add.
   * @return this binding itself.
   *
   * @see #addValidator(Validator)
   */
  public Binding<C, CV, B, BV> setValidators(List<Validator<BV>> validators) {
    this.validators.clear();
    this.validators.addAll(validators);
    return this;
  }

  /**
   * Gets the list of validators.
   *
   * @return the list of validators.
   */
  public List<Validator<BV>> getValidators() {
    return validators;
  }

  /**
   * Validates the value of of the component.
   *
   * @param report Whether to report the validation result using the validation reporter.
   * @param value The value to validate.
   *
   * @return the validation result.
   */
  public ValidationResult validate(CV value, boolean report) {
    if (isReadOnly()) {
      return ValidationResult.valid();
    }

    cachedValue = value;
    isValueCached = true;
    ValidationResult result;

    try {
      BV transformedValue = null;
      transformedValue =
          getTransformer().map(t -> t.transformToModel(value)).orElse(tryCastingToBeanValue(value));

      List<String> messages = new ArrayList<>();
      for (Validator<BV> validator : validators) {
        if (validator instanceof BeanAware) {
          ((BeanAware<B>) validator).setBeanClass(beanClass);
        }

        if (validator instanceof PropertyNameAware pa) {
          pa.setPropertyName(property);
        }

        result = validator.validate(transformedValue);
        if (!result.isValid()) {
          messages.addAll(result.getMessages());
          break;
        }
      }

      if (messages.isEmpty()) {
        result = ValidationResult.valid();
      } else {
        result = ValidationResult.invalid(messages);
      }

    } catch (TransformationException e) {
      result = ValidationResult
          .invalid(transformerMessage == null || transformerMessage.isEmpty() ? e.getMessage()
              : transformerMessage);
    }

    if (validateReporter != null && report) {
      validateReporter.report(result, this);
    }

    return result;
  }

  /**
   * Validates the value of of the component.
   *
   * @param report Whether to report the validation result using the validation reporter.
   *
   * @return the validation result.
   */
  public ValidationResult validate(boolean report) {
    return validate(component.getValue(), report);
  }

  /**
   * Validates the value of of the component.
   *
   * @return the validation result.
   */
  public ValidationResult validate() {
    return validate(true);
  }

  /**
   * Adds a {@link BindingValidateEvent} listener.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<BindingValidateEvent<C, CV, B, BV>> addValidateListener(
      EventListener<BindingValidateEvent<C, CV, B, BV>> listener) {
    return dispatcher.addListener(BindingValidateEvent.class, listener);
  }

  /**
   * Alias for {@link #addValidateListener(EventListener)}.
   *
   * @param listener the event listener to be added
   * @return @return A registration object for removing the event listener
   */
  public ListenerRegistration<BindingValidateEvent<C, CV, B, BV>> onValidate(
      EventListener<BindingValidateEvent<C, CV, B, BV>> listener) {
    return addValidateListener(listener);
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
  public Binding<C, CV, B, BV> setAutoValidate(boolean autoValidate) {
    if (valueChangeListener == null) {
      valueChangeListener = component.addValueChangeListener(this::handleValueChange);
    }

    this.autoValidate = autoValidate;
    updateClientAutoValidation(!autoValidate);

    return this;
  }

  /**
   * Checks if the binding is set to auto-validate.
   *
   * @return true if the binding is set to auto-validate, false otherwise.
   */
  public boolean isAutoValidate() {
    return autoValidate;
  }

  /**
   * Sets the auto-write state of the binding.
   *
   * <p>
   * When the binding is set to auto-write, the value of the component will be written to the bean
   * automatically on each change when it is valid. This is useful when the component is used to
   * display the value and should be updated in the bean immediately.
   * </p>
   *
   * @param bean The bean instance to write to.
   * @return this binding itself.
   */
  public Binding<C, CV, B, BV> setAutoWrite(B bean) {
    if (valueChangeListener == null) {
      valueChangeListener = component.addValueChangeListener(this::handleValueChange);
    }

    this.autoWrite = true;
    this.autoWriteBean = bean;
    return this;
  }

  /**
   * Checks if the binding is set to auto-write.
   *
   * @return true if the binding is set to auto-write, false otherwise.
   */
  public boolean isAutoWrite() {
    return autoWrite;
  }

  /**
   * Sets the validation reporter.
   *
   * <p>
   * The validation reporter is used to report the validation result to the UI component or any
   * other interested component.
   * </p>
   *
   * @param reporter The reporter to add.
   * @return this binding itself.
   */
  public Binding<C, CV, B, BV> setReporter(BindingReporter<C, CV, B, BV> reporter) {
    this.validateReporter = reporter;
    return this;
  }

  /**
   * Retrieves the validation reporter.
   *
   * @return the validation reporter.
   */
  public BindingReporter<C, CV, B, BV> getReporter() {
    return validateReporter;
  }

  /**
   * Populates the component with the value obtained from the bean getter.
   *
   * <p>
   * This method is used to synchronize the component's display with the current property value in
   * the bean.
   * </p>
   *
   * @param bean The bean instance to read from.
   * @throws TransformationException if the value cannot be transformed to the component's type.
   */
  public Binding<C, CV, B, BV> read(B bean) {
    doGetGetter(bean).ifPresent(g -> {
      BV value = g.apply(bean);
      CV transformedValue = getTransformer().map(t -> t.transformToComponent(value))
          .orElseGet(() -> tryCastingToComponentValue(value));
      component.setValue(transformedValue);
    });
    return this;
  }

  /**
   * Updates the bean property with the value obtained from the component.
   *
   * <p>
   * This method is used to synchronize the bean's property with the current value displayed in the
   * component.
   * </p>
   *
   * @param bean The bean instance to write to.
   * @param skipValidation Whether to skip validation before updating the bean.
   */
  public ValidationResult write(B bean, boolean skipValidation) {
    if (isReadOnly()) {
      return ValidationResult.valid();
    }

    final ValidationResult[] result = {ValidationResult.valid()};
    doGetSetter(bean).ifPresent(s -> {
      if (!isValueCached) {
        cachedValue = component.getValue();
      }

      try {
        result[0] = validate(cachedValue, true);
        if (result[0].isValid()) {
          BV transformedValue = getTransformer().map(t -> t.transformToModel(cachedValue))
              .orElseGet(() -> tryCastingToBeanValue(cachedValue));
          s.accept(bean, transformedValue);
        }
      } catch (TransformationException e) {
        result[0] = ValidationResult
            .invalid(transformerMessage == null || transformerMessage.isEmpty() ? e.getMessage()
                : transformerMessage);
      }

      isValueCached = false;
    });

    return result[0];
  }

  /**
   * Updates the bean property with the value obtained from the component.
   *
   * <p>
   * This method is used to synchronize the bean's property with the current value displayed in the
   * component. Validation will be performed before updating the bean.
   * </p>
   *
   * @param bean The bean instance to write to.
   */
  public ValidationResult write(B bean) {
    return write(bean, false);
  }

  void handleValueChange(ValueChangeEvent<CV> event) {
    boolean shouldAutoValidate = isAutoValidate();
    boolean shouldAutoWrite = isAutoWrite();
    ValidationResult result = null;
    CV value = event.getValue();

    if (shouldAutoValidate || shouldAutoWrite) {
      result = validate(value, shouldAutoValidate);
    }

    if (shouldAutoWrite) {
      write(autoWriteBean, true);
    }

    if (result != null) {
      dispatcher.dispatchEvent(new BindingValidateEvent<>(this, result, value));
    }
  }

  private Optional<Function<B, BV>> doGetGetter(B bean) {
    if (getter != null) {
      return getGetter();
    }

    Class<?> bc = bean.getClass();
    if (!bc.equals(lastBeanClassForGetter)) {
      cachedGetter = resolveGetter(bc);
      lastBeanClassForGetter = bc;
    }

    return Optional.ofNullable(cachedGetter);
  }

  @SuppressWarnings("unchecked")
  private Function<B, BV> resolveGetter(Class<?> beanClass) {
    try {
      PropertyDescriptor pd = BeanUtils.getPropertyDescriptor(property, beanClass);
      return b -> {
        try {
          return (BV) pd.getReadMethod().invoke(b);
        } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
          throw new MutatorException(
              "Failed to invoke getter for property '" + property + "' on bean class '" // NOSONAR
                  + beanClass.getName()
                  + "'. Ensure the property name is correct and the getter method is accessible.",
              e);

        }
      };
    } catch (IntrospectionException e) {
      throw new MutatorException("Failed to resolve getter for property '" + property
          + "' on bean class '" + beanClass.getName()
          + "'. Ensure the property name is correct and the getter method is accessible.", e);
    }
  }

  private Optional<BiConsumer<B, BV>> doGetSetter(B bean) {
    if (setter != null) {
      return getSetter();
    }

    Class<?> bc = bean.getClass();
    if (!bc.equals(lastBeanClassForSetter)) {
      cachedSetter = resolveSetter(bc);
      lastBeanClassForSetter = bc;
    }

    return Optional.ofNullable(cachedSetter);
  }

  private BiConsumer<B, BV> resolveSetter(Class<?> beanClass) {
    try {
      PropertyDescriptor pd = BeanUtils.getPropertyDescriptor(property, beanClass);
      return (b, v) -> {
        try {
          pd.getWriteMethod().invoke(b, v);
        } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
          throw new MutatorException(
              "Failed to invoke setter for property '" + property + "' on bean class '"
                  + beanClass.getName()
                  + "'. Ensure the property name is correct and the setter method is accessible.",
              e);
        }
      };
    } catch (IntrospectionException e) {
      throw new MutatorException("Failed to resolve setter for property '" + property
          + "' on bean class '" + beanClass.getName()
          + "'. Ensure the property name is correct and the setter method is accessible.", e);
    }
  }

  @SuppressWarnings("unchecked")
  private BV tryCastingToBeanValue(Object value) {
    try {
      return (BV) value;
    } catch (ClassCastException e) {
      throw new TransformationException("Failed to cast value to model type.", e);
    }
  }

  @SuppressWarnings("unchecked")
  private CV tryCastingToComponentValue(Object value) {
    try {
      return (CV) value;
    } catch (ClassCastException e) {
      throw new TransformationException("Failed to cast value to view type.", e);
    }
  }

  private void updateClientAutoValidation(boolean enabled) {
    if (component instanceof AutoClientValidation) {
      @SuppressWarnings("unchecked")
      AutoClientValidation<C> avc = (AutoClientValidation<C>) component;
      avc.setAutoClientValidate(enabled);
    }

    if (component instanceof AutoClientValidationOnLoad) {
      @SuppressWarnings("unchecked")
      AutoClientValidationOnLoad<C> avc = (AutoClientValidationOnLoad<C>) component;
      avc.setAutoClientValidateOnLoad(enabled);
    }
  }


  private void autoDetectedRequired() {
    if (property == null || property.isEmpty()) {
      return;
    }

    try {
      Field field = beanClass.getDeclaredField(property);
      if (hasValidationAnnotations(field)) {
        setRequired(true);
      }
    } catch (NoSuchFieldException e) {
      throw new IllegalArgumentException("Property '" + property + "' not found in bean class '"
          + beanClass.getName() + "'. Ensure the property name is correct.");
    } catch (SecurityException e) {
      throw new IllegalArgumentException("Failed to access property '" + property
          + "' in bean class '" + beanClass.getName() + "'. Ensure the property is accessible.");
    }
  }

  private static boolean hasValidationAnnotations(Field field) {
    String[] validationAnnotations =
        {"jakarta.validation.constraints.NotNull", "jakarta.validation.constraints.NotEmpty",
            "jakarta.validation.constraints.NotBlank", "jakarta.validation.constraints.Size"};

    for (String annotationClassName : validationAnnotations) {
      try {
        Class<?> annotationClass = Class.forName(annotationClassName);
        if (field.isAnnotationPresent((Class<Annotation>) annotationClass)) {
          return true;
        }
      } catch (ClassNotFoundException e) {
        // Annotation class not found, continue
      }
    }

    return false;
  }
}
