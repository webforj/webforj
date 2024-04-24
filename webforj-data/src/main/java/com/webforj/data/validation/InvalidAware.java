package com.webforj.data.validation;

/**
 * Represents a component that can be marked as invalid.
 *
 * <p>
 * This interface is used to mark components that can be marked as invalid. An invalid component is
 * a component that has a validation error.
 * </p>
 *
 * @param <T> the type of the component.
 *
 * @author Hyyan Abo Fakher
 * @since 24.01
 */
public interface InvalidAware<T> {

  /**
   * Marks the component as invalid.
   *
   * @param invalid true to mark the component as invalid, false otherwise.
   * @return this component itself.
   */
  public T setInvalid(boolean invalid);

  /**
   * Returns whether the component is invalid.
   *
   * @return true if the component is invalid, false otherwise.
   */
  public boolean isInvalid();

  /**
   * Sets the invalid message of the component.
   *
   * @param message the invalid message.
   * @return this component itself.
   */
  public T setInvalidMessage(String message);

  /**
   * Returns the invalid message of the component.
   *
   * @return the invalid message.
   */
  public String getInvalidMessage();
}
