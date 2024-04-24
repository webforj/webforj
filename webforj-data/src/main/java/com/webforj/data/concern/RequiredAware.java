package com.webforj.data.concern;

/**
 * An interface for implementing methods that allow toggling the required status on a component.
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public interface RequiredAware<T> {

  /**
   * Sets whether the component is required to have a value.
   *
   * @param required true if the component must have a value, false if the component may be empty.
   * @return the component itself after configuring the required status.
   */
  public T setRequired(boolean required);

  /**
   * Checks whether the component is required to have a value.
   *
   * @return true if the component must have a value, false if the component may be empty.
   */
  public boolean isRequired();
}
