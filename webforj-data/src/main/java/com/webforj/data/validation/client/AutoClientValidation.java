package com.webforj.data.validation.client;

/**
 * Represents a component that can be automatically validated when the value changes.
 *
 * @param <T> the type of the component.
 *
 * @since 24.01
 * @author Hyyan Abo Fakher
 */
public interface AutoClientValidation<T> {

  /**
   * When true the component will automatically validate itself when the value changes based on the
   * configured client validator and constraints.
   *
   * @param autoValidate true to enable auto validation, false otherwise.
   * @return the component.
   */
  public T setAutoClientValidate(boolean autoValidate);

  /**
   * Returns true if the component will automatically validate itself when the value changes.
   *
   * @return true if auto validation is enabled, false otherwise.
   */
  public boolean isAutoClientValidate();
}
