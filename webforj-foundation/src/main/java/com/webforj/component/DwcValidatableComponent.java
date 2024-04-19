package com.webforj.component;

import com.webforj.concern.HasClientAutoValidation;
import com.webforj.concern.HasClientAutoValidationOnLoad;
import com.webforj.concern.HasClientValidation;
import com.webforj.concern.HasClientValidationStyle;
import com.webforj.concern.HasValue;
import com.webforj.data.validation.client.ClientValidator;

/**
 * The base class for all DWC validatable components.
 *
 * <p>
 * This abstract class serves as the foundation for all validatable components within the framework.
 * It extends the {@link DwcFocusableComponent} class and implements several interfaces for handling
 * validatable-specific properties and behaviors defined by DWC.
 * </p>
 *
 * <p>
 * <strong>Important:</strong> Same inheritance rules of {@link DwcComponent} apply to this class.
 * </p>
 *
 * @param <T> The type of the component.
 * @param <V> The type of value associated with the component.
 *
 * @see DwcFocusableComponent
 * @see HasValue
 * @see HasClientValidation
 *
 * @author Hyyan Abo Fakher
 * @since 24.01
 */
public abstract class DwcValidatableComponent<T extends DwcFocusableComponent<T>, V>
    extends DwcFocusableComponent<T> implements HasValue<T, V>, HasClientValidation<T>,
    HasClientAutoValidation<T>, HasClientAutoValidationOnLoad<T>, HasClientValidationStyle<T> {

  private boolean autoValidate = true;
  private boolean autoValidateOnLoad = false;
  private ClientValidator validator;
  private String invalidMessage;
  private ValidationStyle validationStyle = ValidationStyle.POPOVER;

  /**
   * {@inheritDoc}
   */
  @Override
  public T setAutoClientValidateOnLoad(boolean autoValidateOnLoad) {
    this.autoValidateOnLoad = autoValidateOnLoad;
    setUnrestrictedProperty("autoValidateOnLoad", this.autoValidateOnLoad);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isAutoClientValidateOnLoad() {
    return this.autoValidateOnLoad;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T setAutoClientValidate(boolean autoValidate) {
    this.autoValidate = autoValidate;
    setUnrestrictedProperty("autoValidate", this.autoValidate);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isAutoClientValidate() {
    return this.autoValidate;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T setValidationStyle(ValidationStyle validationStyle) {
    this.validationStyle = validationStyle;
    setUnrestrictedProperty("validationStyle", validationStyle);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ValidationStyle getValidationStyle() {
    return this.validationStyle;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T setInvalid(boolean invalid) {
    setUnrestrictedProperty("invalid", invalid);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isInvalid() {
    return getProperty(invalidMessage, Boolean.class);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T setInvalidMessage(String message) {
    this.invalidMessage = message;
    setUnrestrictedProperty("invalidMessage", this.invalidMessage);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getInvalidMessage() {
    return this.invalidMessage;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T setClientValidator(ClientValidator validator) {
    this.validator = validator;
    setUnrestrictedProperty("validator", this.validator.getExpression());
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ClientValidator getClientValidator() {
    return this.validator;
  }
}
