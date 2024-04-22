package com.webforj.data.binding;

import com.webforj.data.binding.annotation.BindingExclude;
import com.webforj.data.binding.annotation.UseProperty;
import com.webforj.data.binding.annotation.UseTransformer;
import com.webforj.data.binding.annotation.UseValidator;
import com.webforj.data.binding.concern.AutomaticBindAware;
import com.webforj.data.binding.concern.BindAware;
import com.webforj.data.binding.concern.UnbindAware;
import com.webforj.data.binding.event.BindingContextStatusEvent;
import com.webforj.data.concern.FocusAcceptorAware;
import com.webforj.data.concern.ValueAware;
import com.webforj.data.transformation.transformer.Transformer;
import com.webforj.data.validation.server.ValidationResult;
import com.webforj.data.validation.server.validator.JakartaValidator;
import com.webforj.data.validation.server.validator.Validator;
import com.webforj.dispatcher.EventDispatcher;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.util.AbstractMap.SimpleImmutableEntry;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.function.Predicate;

/**
 * Represents a context for managing a collection of {@link Binding} instances.
 *
 * <p>
 * BindingContext manages a collection of {@link Binding} instances, each linking a UI component to
 * a property of a bean. This class facilitates group operations on bindings, such as validation and
 * synchronization between the UI components and the bean's properties. It acts as an aggregator,
 * allowing for collective actions on multiple bindings, thereby streamlining the management of data
 * flow within applications.
 * </p>
 *
 * <p>
 * Through the use of {@code BindingContext}, applications can maintain a clean separation between
 * the presentation layer and the model, ensuring that changes in the UI are automatically reflected
 * in the model and vice versa. This leads to a reduction in boilerplate code and enhances the
 * maintainability and scalability of the application.
 * </p>
 *
 * @param <B> the type of the bean which the bindings are synchronized with.
 *
 * @since 24.01
 * @author Hyyan Abo Fakher
 */
public class BindingContext<B> {
  private final EventDispatcher dispatcher = new EventDispatcher();
  private final Class<B> beanClass;
  private final Map<String, Binding<?, ?, B, ?>> bindings = new LinkedHashMap<>();
  private final Map<Binding<?, ?, B, ?>, ValidationResult> validationResults =
      new LinkedHashMap<>();
  private boolean globalUseJakartaValidator = false;
  private boolean globalAutoValidate = true;
  private boolean autoFocusFirstViolation = true;
  private boolean temporaryDisableStatusEvent = false;

  /**
   * Creates a new instance of {@code BindingContext}.
   *
   * @param beanClass The class of the bean to bind
   * @param useJakartaValidator Whether to use Jakarta Bean Validator for all the bindings in the
   *        context
   */
  public BindingContext(Class<B> beanClass, boolean useJakartaValidator) {
    this.beanClass = Objects.requireNonNull(beanClass, "beanClass cannot be null");
    this.globalUseJakartaValidator = useJakartaValidator;
  }

  /**
   * Creates a new instance of {@code BindingContext}.
   *
   * @param beanClass The class of the bean to bind
   */
  public BindingContext(Class<B> beanClass) {
    this(beanClass, false);
  }

  /**
   * Creates a new instance of {@code BindingContext} by establishing bindings between the given
   * Object and the specified bean class.
   *
   * <p>
   * This method automatically aligns the bindable components declared as fields in the object with
   * the properties of the bean, based on their names. The name of field should match the name of
   * the property in the bean. If the field name differs from the property name, the
   * {@link UseProperty} annotation can be used to specify the property name. The
   * {@link BindingExclude} annotation can be used to exclude a field from being bound.
   * </p>
   *
   * @param <B> the type of the bean with which the bindings will be synchronized
   * @param object the object containing bindable components
   * @param beanClass the class of the bean to which bindings should be established
   * @param useJakartaValidator whether to use Jakarta Bean Validator for all the bindings in the
   *
   * @return a new {@code BindingContext} instance
   */
  @SuppressWarnings({"unchecked"})
  public static <B> BindingContext<B> of(Object object, Class<B> beanClass,
      boolean useJakartaValidator) {
    BindingContext<B> context = new BindingContext<>(beanClass, useJakartaValidator);

    Field[] components = object.getClass().getDeclaredFields();
    for (Field field : components) {
      if (field.isAnnotationPresent(BindingExclude.class)) {
        continue; // Skip this field
      }

      String name = field.getName();
      UseProperty bindToAnnotation = field.getAnnotation(UseProperty.class);

      if (bindToAnnotation != null) {
        name = bindToAnnotation.value();
      }

      Object fieldValue = null;
      try {
        field.setAccessible(true); // NOSONAR
        fieldValue = field.get(object);
      } catch (IllegalAccessException e) {
        throw new IllegalStateException("Cannot access field '" + name + "' in object of class '"
            + object.getClass().getSimpleName()
            + "'. Ensure the field is accessible and not private, "
            + "or annotate it properly to be included/excluded in binding.");
      }

      // check if the field value is a ValueAware component
      if (fieldValue instanceof ValueAware valueAware) {
        // build the transformer config
        SimpleImmutableEntry<Transformer<?, ?>, String> transformerConfig =
            processUseTransformerAnnotation(field);
        Transformer<?, ?> transformer =
            Optional.ofNullable(transformerConfig).map(SimpleImmutableEntry::getKey).orElse(null);
        String transformerMessage =
            Optional.ofNullable(transformerConfig).map(SimpleImmutableEntry::getValue).orElse(null);

        // build the validators config
        List<SimpleImmutableEntry<Validator<?>, String>> validatorsConfig =
            processUseValidatorAnnotations(field);
        List<?> validators = validatorsConfig.stream()
            .map(entry -> Optional.ofNullable(entry).map(SimpleImmutableEntry::getKey).orElse(null))
            .toList();
        List<String> messages = validatorsConfig.stream().map(
            entry -> Optional.ofNullable(entry).map(SimpleImmutableEntry::getValue).orElse(null))
            .toList();

        // establish the binding
        context.bind(valueAware, name).useTransformer(transformer, transformerMessage)
            .useValidatorsList(validators, messages).add();

        if (valueAware instanceof AutomaticBindAware automaticBindAware) {
          automaticBindAware.onAutomaticBind(context, beanClass, name, field);
        }
      }
    }

    return context;
  }

  /**
   * Creates a new instance of {@code BindingContext} by establishing bindings between the given
   * {@link ValueAware} component and the specified bean class.
   *
   * <p>
   * This method automatically aligns the bindable components of the object with the properties of
   * the bean, based on their names.
   * </p>
   *
   * @param <B> the type of the bean with which the bindings will be synchronized
   * @param object the object containing bindable components
   * @param beanClass the class of the bean to which bindings should be established
   *
   * @return a new {@code BindingContext} instance
   */
  public static <B> BindingContext<B> of(Object object, Class<B> beanClass) {
    return of(object, beanClass, false);
  }

  /**
   * Adds a binding to the context.
   *
   * @param <C> The type of the UI component
   * @param <V> The type of the value
   * @param <BV> The type of the bean value
   *
   * @param component The UI component to bind
   * @param property The bean property to bind
   * @param beanValueClass The class of the bean value
   *
   * @return The binding context.
   */
  public <C extends ValueAware<C, V>, V, BV> BindingBuilder<C, V, B, BV> bind(C component,
      String property, Class<BV> beanValueClass) {
    return new FieldBindingBuilderImpl<>(component, property);
  }

  /**
   * Adds a binding to the context.
   *
   * @param <C> The type of the UI component
   * @param <V> The type of the value
   * @param component The UI component to bind
   * @param property The bean property to bind
   *
   * @return The binding context.
   */
  public <C extends ValueAware<C, V>, V> BindingBuilder<C, V, B, V> bind(C component,
      String property) {
    return bind(component, property, null);
  }

  /**
   * Removes a binding from the context based on the specified property.
   *
   * @param property The bean property to bind
   * @return The binding context.
   */
  public BindingContext<B> unbind(String property) {
    ValueAware<?, ?> component = bindings.get(property).getComponent();
    bindings.remove(property);

    if (component instanceof UnbindAware unbindAware) {
      unbindAware.onUnbind(this, beanClass, property);
    }

    return this;
  }

  /**
   * Removes a binding from the context based on the specified component.
   *
   * @param <C> The type of the UI component
   * @param <V> The type of the value
   * @param component The UI component to unbind
   *
   * @return The binding context.
   */
  public <C extends ValueAware<C, V>, V> BindingContext<B> unbind(C component) {
    Map<String, Binding<?, ?, B, ?>> clone = new HashMap<>(this.bindings);
    clone.forEach((property, binding) -> {
      if (binding.getComponent().equals(component)) {
        unbind(property);
      }
    });

    return this;
  }

  /**
   * Checks if the specified property is bound in the context.
   *
   * @param property The bean property to bind
   * @return {@code true} if the property is bound, {@code false} otherwise.
   */
  public boolean isBound(String property) {
    return bindings.containsKey(property);
  }

  /**
   * Checks if the specified component is bound in the context.
   *
   * @param <C> The type of the UI component
   * @param <V> The type of the value
   * @param component The UI component to bind
   * @return {@code true} if the component is bound, {@code false} otherwise.
   */
  public <C extends ValueAware<C, V>, V> boolean isBound(C component) {
    return bindings.values().stream().anyMatch(b -> b.getComponent().equals(component));
  }

  /**
   * Gets the binding associated with the specified property.
   *
   * @param property The bean property to bind
   * @return the binding associated with the specified property.
   */
  public Binding<?, ?, B, ?> getBinding(String property) {
    return bindings.get(property);
  }

  /**
   * Gets the binding associated with the specified component.
   *
   * @param <C> The type of the UI component
   * @param <V> The type of the value
   * @param component The UI component to bind
   * @return the binding associated with the specified component.
   */
  public <C extends ValueAware<C, V>, V> Binding<?, ?, B, ?> getBinding(C component) {
    return bindings.values().stream().filter(b -> b.getComponent().equals(component)).findFirst()
        .orElse(null);
  }

  /**
   * Validates all the bindings in the context.
   *
   * @param report whether to report the validation result using the {@link BindingReporter}
   * @return the validation result.
   */
  public ValidationResult validate(boolean report) {
    List<String> messages = new ArrayList<>();
    boolean isValid = true;

    ValueAware<?, ?> firstInvalidComponent = null;
    for (Binding<?, ?, B, ?> binding : bindings.values()) {
      ValidationResult result = binding.validate(report);
      if (!result.isValid()) {
        if (firstInvalidComponent == null) {
          firstInvalidComponent = binding.getComponent();
        }

        isValid = false;
        messages.addAll(result.getMessages());
      }
    }

    if (!isValid && isAutoFocusFirstViolation() && firstInvalidComponent != null
        && firstInvalidComponent instanceof FocusAcceptorAware focusAcceptorAware) {
      focusAcceptorAware.focus();
    }

    return isValid ? ValidationResult.valid() : ValidationResult.invalid(messages);
  }

  /**
   * Validates all the bindings in the context.
   *
   * @return the validation result.
   */
  public ValidationResult validate() {
    return validate(true);
  }

  /**
   * Validates the binding associated with the specified component.
   *
   * @param <C> The type of the UI component
   * @param <V> The type of the value
   * @param component The UI component to bind
   * @param report whether to report the validation result using the {@link BindingReporter}
   *
   * @return the validation result.
   */
  public <C extends ValueAware<C, V>, V> ValidationResult validate(C component, boolean report) {
    Binding<?, ?, B, ?> binding = bindings.values().stream()
        .filter(b -> b.getComponent().equals(component)).findFirst().orElse(null);

    return binding != null ? binding.validate(report) : ValidationResult.valid();
  }

  /**
   * Validates the binding associated with the specified component.
   *
   * @param <C> The type of the UI component
   * @param <V> The type of the value
   * @param component The UI component to bind
   *
   * @return the validation result.
   */
  public <C extends ValueAware<C, V>, V> ValidationResult validate(C component) {
    return validate(component, true);
  }

  /**
   * Validates the binding associated with the specified property.
   *
   * @param property The bean property to bind
   * @param report whether to report the validation result using the {@link BindingReporter}
   *
   * @return the validation result.
   */
  public ValidationResult validate(String property, boolean report) {
    Binding<?, ?, B, ?> binding = bindings.get(property);
    return binding != null ? binding.validate(report) : ValidationResult.valid();
  }

  /**
   * Validates the binding associated with the specified property.
   *
   * @param property The bean property to bind
   * @return the validation result.
   */
  public ValidationResult validate(String property) {
    return validate(property, true);
  }

  /**
   * Checks if all the bindings in the context are valid.
   *
   * @return {@code true} if the bindings are valid, {@code false} otherwise.
   */
  public boolean isValid() {
    return validate(false).isValid();
  }

  /**
   * Checks if the binding associated with the specified component is valid.
   *
   * @return {@code true} if the binding is valid, {@code false} otherwise.
   */
  public <C extends ValueAware<C, V>, V> boolean isValid(C component) {
    return validate(component, false).isValid();
  }

  /**
   * Checks if the binding associated with the specified property is valid.
   *
   * @return {@code true} if the binding is valid, {@code false} otherwise.
   */
  public boolean isValid(String property) {
    return validate(property, false).isValid();
  }

  /**
   * Adds a {@link BindingContextStatusEvent} listener.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<BindingContextStatusEvent<B>> addStatusChangeListener(
      EventListener<BindingContextStatusEvent<B>> listener) {
    return dispatcher.addListener(BindingContextStatusEvent.class, listener);
  }

  /**
   * Alias for {@link #addStatusChangeListener(EventListener)}.
   *
   * @param listener the event listener to be added
   * @return @return A registration object for removing the event listener
   */
  public ListenerRegistration<BindingContextStatusEvent<B>> onStatusChange(
      EventListener<BindingContextStatusEvent<B>> listener) {
    return addStatusChangeListener(listener);
  }

  /**
   * Populates the bound components with the value obtained from the bean properties.
   *
   * @param bean The bean to sync from
   * @return the binding context.
   */
  public BindingContext<B> read(B bean) {
    bindings.values().forEach(binding -> binding.read(bean));
    return this;
  }

  /**
   * Updates the bean properties with the values obtained from the bound components.
   *
   * @param bean The bean to sync to
   * @return the validation result.
   */
  public ValidationResult write(B bean) {
    this.temporaryDisableStatusEvent = true;
    ValidationResult result = validate();
    if (result.isValid()) {
      bindings.values().forEach(binding -> binding.write(bean, true));
    }
    this.temporaryDisableStatusEvent = false;

    return result;
  }

  /**
   * Updates the bean properties with the values obtained from the bound components that passes the
   * validation.
   *
   * @param bean The bean to sync to
   * @return the validation result.
   */
  public BindingContext<B> writeValidated(B bean) {
    this.temporaryDisableStatusEvent = true;
    bindings.values().forEach(binding -> binding.write(bean, false));
    this.temporaryDisableStatusEvent = false;
    return this;
  }

  /**
   * Configures the binding context to focus on the first component with a validation error.
   *
   * @param autoFocusFirstViolation whether to focus on the first component with a validation error
   * @return the binding context.
   */
  public BindingContext<B> setAutoFocusFirstViolation(boolean autoFocusFirstViolation) {
    this.autoFocusFirstViolation = autoFocusFirstViolation;
    return this;
  }

  /**
   * Checks if the binding context is configured to focus on the first component with a validation
   * error.
   *
   * @return {@code true} if the binding context is configured to focus on the first component with
   *         a validation error, {@code false} otherwise.
   */
  public boolean isAutoFocusFirstViolation() {
    return autoFocusFirstViolation;
  }

  /**
   * Configures the binding context to automatically validate all the bindings in the context when
   * the value of a component changes.
   *
   * @param autoValidate whether to automatically validate all the bindings in the context when the
   *        value of a component changes
   * @return the binding context.
   */
  public BindingContext<B> setAutoValidate(boolean autoValidate) {
    this.globalAutoValidate = autoValidate;
    bindings.values().forEach(binding -> binding.setAutoValidate(autoValidate));
    return this;
  }

  /**
   * Checks if the binding context is configured to automatically validate all the bindings in the
   * context when the value of a component changes.
   *
   * @return {@code true} if the binding context is configured to automatically validate all the
   *         bindings in the context when the value of a component changes, {@code false} otherwise.
   */
  public boolean isAutoValidate() {
    return globalAutoValidate;
  }

  private static SimpleImmutableEntry<Transformer<?, ?>, String> processUseTransformerAnnotation(
      Field field) {
    if (field.isAnnotationPresent(UseTransformer.class)) {
      UseTransformer useTransformerAnnotation = field.getAnnotation(UseTransformer.class);
      // Dynamically instantiate the transformer
      try {
        Transformer<?, ?> transformer =
            useTransformerAnnotation.value().getDeclaredConstructor().newInstance();
        String message = useTransformerAnnotation.message();

        return new SimpleImmutableEntry<>(transformer, message);
      } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
          | InvocationTargetException | NoSuchMethodException | SecurityException e) {
        throw new IllegalStateException("Cannot instantiate transformer class '"
            + useTransformerAnnotation.value().getSimpleName() + "'. Ensure the class has a "
            + "public no-arg constructor.");
      }
    }

    return null;
  }

  private static List<SimpleImmutableEntry<Validator<?>, String>> processUseValidatorAnnotations(
      Field field) {
    List<SimpleImmutableEntry<Validator<?>, String>> results = new ArrayList<>();
    UseValidator[] annotations = field.getAnnotationsByType(UseValidator.class);

    for (UseValidator annotation : annotations) {
      Class<? extends Validator<?>> validatorClass = annotation.value();

      try {
        Validator<?> validator = validatorClass.getDeclaredConstructor().newInstance();
        String message = annotation.message();

        results.add(new SimpleImmutableEntry<>(validator, message));
      } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
          | InvocationTargetException | NoSuchMethodException | SecurityException e) {
        throw new IllegalStateException(
            "Cannot instantiate validator class '" + validatorClass.getSimpleName()
                + "'. Ensure the class has a public no-arg " + "constructor.");
      }

    }

    return results;
  }

  private final class FieldBindingBuilderImpl<C extends ValueAware<C, CV>, CV, BV>
      implements BindingBuilder<C, CV, B, BV> {

    private Binding<C, CV, B, BV> fieldBinding;
    private String property;
    private C component;
    private boolean isUsingJakartaValidator = false;

    FieldBindingBuilderImpl(C component, String property) {
      if (isBound(component)) {
        throw new IllegalStateException("Component '" + component.getClass().getSimpleName()
            + "' is already bound to a property. "
            + "Each component can only be bound to one property at a time.");
      }

      if (isBound(property)) {
        throw new IllegalStateException(
            "Property '" + property + "' is already bound to a component. "
                + "Each property can only be bound to one component at a time.");
      }

      this.component = component;
      this.property = property;

      fieldBinding = new Binding<>(component, beanClass, property);
      useDefaultReporter();

      if (globalAutoValidate) {
        autoValidate();
      }

      if (globalUseJakartaValidator) {
        useJakartaValidator();
      }

      fieldBinding.onValidation(event -> {
        validationResults.put(fieldBinding, event.getValidationResult());

        boolean isContextValid =
            validationResults.values().stream().allMatch(ValidationResult::isValid);
        ValidationResult result = isContextValid ? ValidationResult.valid()
            : ValidationResult.invalid(validationResults.values().stream()
                .flatMap(vr -> vr.getMessages().stream()).toList());

        if (!temporaryDisableStatusEvent) {
          dispatcher.dispatchEvent(new BindingContextStatusEvent<>(BindingContext.this, result));
        }
      });
    }

    @Override
    public BindingBuilder<C, CV, B, BV> useGetter(Function<B, BV> getter) {
      fieldBinding.setGetter(getter);
      return this;
    }

    @Override
    public BindingBuilder<C, CV, B, BV> useSetter(BiConsumer<B, BV> setter) {
      fieldBinding.setSetter(setter);
      return this;
    }

    @Override
    public BindingBuilder<C, CV, B, BV> useTransformer(Transformer<CV, BV> transformer,
        String message) {
      fieldBinding.setTransformer(transformer, message);
      return this;
    }

    @Override
    public BindingBuilder<C, CV, B, BV> readOnly(boolean readOnly) {
      fieldBinding.setReadOnly(readOnly);
      return this;
    }

    @Override
    public BindingBuilder<C, CV, B, BV> required(boolean required) {
      fieldBinding.setRequired(required);
      return this;
    }

    @Override
    public BindingBuilder<C, CV, B, BV> useValidator(Validator<BV> validator) {
      fieldBinding.addValidator(validator);
      return this;
    }

    @Override
    public BindingBuilder<C, CV, B, BV> useValidator(Predicate<BV> validator, String message) {
      fieldBinding.addValidator(Validator.of(validator, message));
      return this;
    }

    @Override
    public BindingBuilder<C, CV, B, BV> useValidatorsList(List<Validator<BV>> validators,
        List<String> messages) {
      List<String> messagesCopy = new ArrayList<>(messages);

      for (Validator<BV> validator : validators) {
        if (messagesCopy.isEmpty()) {
          fieldBinding.addValidator(validator);
        } else {
          String message = !messagesCopy.isEmpty() ? messagesCopy.remove(0) : "";
          fieldBinding.addValidator(Validator.of(validator, message));
        }
      }

      return this;
    }

    @Override
    public BindingBuilder<C, CV, B, BV> useJakartaValidator(Locale locale) {
      if (!(property != null && !property.isEmpty())) {
        return this;
      }

      if (isUsingJakartaValidator) {
        throw new IllegalStateException(
            "Jakarta Validator is already set up for the FieldBinding associated with property '"
                + property + "'. Each binding can only have one Jakarta Validator.");
      }

      isUsingJakartaValidator = true;
      return useValidator(new JakartaValidator<>(locale));
    }

    @Override
    public BindingBuilder<C, CV, B, BV> useReporter(BindingReporter<C, CV, B, BV> reporter) {
      fieldBinding.setReporter(reporter);
      return this;
    }

    @Override
    public BindingBuilder<C, CV, B, BV> useDefaultReporter(
        Function<List<String>, String> formatter) {
      return useReporter(new DefaultBindingReporter<>(formatter));
    }

    @Override
    public BindingBuilder<C, CV, B, BV> autoValidate(boolean autoValidationOnChange) {
      fieldBinding.setAutoValidate(autoValidationOnChange);
      return this;
    }

    @Override
    public BindingContext<B> add() {
      bindings.put(property, fieldBinding);

      if (component instanceof BindAware bindAware) {
        bindAware.onBind(BindingContext.this, beanClass, property);
      }

      return BindingContext.this;
    }

    @Override
    public void remove() {
      bindings.remove(fieldBinding.getProperty());
    }
  }
}
