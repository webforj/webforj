package com.webforj.data.validation.client;

import com.webforj.data.validation.InvalidAware;

/**
 * Represents a client validatable component.
 *
 * <p>
 * A client validatable component is a component that can be validated on the client side. The
 * validation is typically done by the client-side JavaScript code, which can be used to provide
 * immediate feedback to the user. This interface provides a way to mark a component as invalid, so
 * that the client-side code can highlight the component as such.
 * </p>
 *
 * @param <T> the type of the component implementing this interface.
 *
 * @since 24.01
 * @author Hyyan Abo Fakher
 */
public interface ClientValidatable<T> extends InvalidAware<T> {

  /**
   * Sets the client validator of the component.
   *
   * @param clientValidator the client validator.
   * @return this component itself.
   */
  public T setClientValidator(ClientValidator clientValidator);

  /**
   * Returns the client validator of the component.
   *
   * @return the client validator.
   */
  public ClientValidator getClientValidator();
}
