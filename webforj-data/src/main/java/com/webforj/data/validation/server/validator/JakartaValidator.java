package com.webforj.data.validation.server.validator;

import com.webforj.data.BeanAware;
import com.webforj.data.PropertyNameAware;
import com.webforj.data.validation.server.ValidationResult;
import jakarta.validation.ConstraintViolation;
import jakarta.validation.MessageInterpolator.Context;
import jakarta.validation.Validation;
import jakarta.validation.ValidatorFactory;
import jakarta.validation.metadata.ConstraintDescriptor;
import java.util.Locale;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Adapter for Jakarta Validation API.
 *
 * @param <B> the type of the bean to validate
 * @param <V> the type of the value to validate
 *
 * @author Hyyan Abo Fakher
 * @since 24.01
 */
public class JakartaValidator<B, V> implements Validator<V>, BeanAware<B>, PropertyNameAware {
  private Class<B> beanClass;
  private String propertyName;
  private ValidatorFactory validatorFactory;
  private Locale locale;

  /**
   * Constructs a new instance of the adapter.
   *
   * @param locale the locale to use
   */
  public JakartaValidator(Locale locale) {
    this.locale = locale;
  }

  /**
   * Constructs a new instance of the adapter.
   */
  public JakartaValidator() {
    this(Locale.getDefault());
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ValidationResult validate(V value) {
    if (beanClass == null) {
      throw new IllegalStateException("Bean class not set");
    }

    if (propertyName == null) {
      throw new IllegalStateException("Property name not set");
    }

    Set<ConstraintViolation<B>> violations =
        getValidator().validateValue(beanClass, propertyName, value);

    if (violations.isEmpty()) {
      return ValidationResult.valid();
    } else {
      return ValidationResult.invalid(
          violations.stream().map(v -> getMessage(v, locale)).collect(Collectors.toList()));
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void setPropertyName(String propertyName) {
    this.propertyName = propertyName;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getPropertyName() {
    return propertyName;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void setBeanClass(Class<B> beanClass) {
    this.beanClass = beanClass;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Class<B> getBeanClass() {
    return beanClass;
  }

  /**
   * Gets the validator factory.
   *
   * @return the validator factory
   */
  protected ValidatorFactory getValidatorFactory() {
    if (validatorFactory == null) {
      validatorFactory = Validation.buildDefaultValidatorFactory();
    }

    return validatorFactory;
  }

  /**
   * Gets the validator.
   *
   * @return the validator
   */
  protected jakarta.validation.Validator getValidator() {
    return getValidatorFactory().getValidator();
  }

  /**
   * Gets the message for the given violation.
   *
   * @param violation the violation
   * @param locale the locale
   *
   * @return the message
   */
  protected String getMessage(ConstraintViolation<?> violation, Locale locale) {
    return getValidatorFactory().getMessageInterpolator()
        .interpolate(violation.getMessageTemplate(), new CustomContext(violation), locale);
  }

  private final class CustomContext implements Context {
    private final ConstraintViolation<?> violation;

    private CustomContext(ConstraintViolation<?> violation) {
      this.violation = violation;
    }

    @Override
    public ConstraintDescriptor<?> getConstraintDescriptor() {
      return violation.getConstraintDescriptor();
    }

    @Override
    public Object getValidatedValue() {
      return violation.getInvalidValue();
    }

    @Override
    public <T> T unwrap(Class<T> type) {
      return violation.unwrap(type);
    }
  }
}
