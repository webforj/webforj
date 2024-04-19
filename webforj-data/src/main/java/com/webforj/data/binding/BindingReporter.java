package com.webforj.data.binding;

import com.webforj.data.concern.ValueAware;
import com.webforj.data.validation.server.ValidationResult;

/**
 * Represents a reporter that can be used to report the validation results of a binding.
 *
 * @since 24.01
 * @author Hyyan Abo Fakher
 */
@FunctionalInterface
public interface BindingReporter<C extends ValueAware<C, CV>, CV, B, V> {

  /**
   * Reports the validation result.
   *
   * @param result the validation result.
   * @param binding the binding that was validated.
   */
  void report(ValidationResult result, Binding<C, CV, B, V> binding);
}
