package com.webforj.data.binding.annotation;

import com.webforj.data.validation.server.validator.Validator;
import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotation to specify the validator that should be used to validate the bean value.
 *
 * @author Hyyan Abo Fakher
 * @since 24.01
 */
@Retention(RetentionPolicy.RUNTIME)
@Repeatable(UseValidator.Container.class)
@Target(ElementType.FIELD)
public @interface UseValidator {

  /**
   * The validator class to use for validating the value. The validator class must implement the
   * {@link Validator} interface.
   */
  @SuppressWarnings("squid:S1452")
  Class<? extends Validator<?>> value();

  /**
   * The message to use in case of validation failure.
   */
  String message() default "";

  /**
   * Container annotation to hold multiple validators.
   */
  @Target(ElementType.FIELD)
  @Retention(RetentionPolicy.RUNTIME)
  public @interface Container {
    /**
     * The validators to use for validating the value.
     */
    UseValidator[] value();
  }
}
