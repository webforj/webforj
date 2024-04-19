package com.webforj.data.validation.client;

/**
 * Represents a component that can be automatically validated on load.
 *
 * @param <T> the type of the component.
 *
 * @author Hyyan Abo Fakher
 * @since 24.01
 */
public interface AutoClientValidationOnLoad<T> {

  /**
   * When true the component will automatically validate itself when the component is loaded for the
   * first time based on the configured client validator and constraints.
   *
   * @param autoValidateOnLoad true to enable auto validation on load, false otherwise.
   * @return the component.
   */
  public T setAutoClientValidateOnLoad(boolean autoValidateOnLoad);

  /**
   * Returns true if the component will automatically validate itself when the component is loaded
   * for the first time.
   *
   * @return true if auto validation on load is enabled, false otherwise.
   */
  public boolean isAutoClientValidateOnLoad();
}
