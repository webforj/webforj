package com.webforj.router.exception;

import com.webforj.exceptions.WebforjRuntimeException;

/**
 * Represents an exception that occurs when a route is not found.
 *
 * <p>
 * Route not found exceptions are thrown when a route is not found in the route registry or when the
 * target component of the route is not found.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.12
 */
public class NotFoundException extends WebforjRuntimeException {

  /**
   * Constructs a new {@code RouteNotFoundException} with the given message.
   *
   * @param message the exception message
   */
  public NotFoundException(String message) {
    super(message);
  }

  /**
   * Constructs a new {@code RouteNotFoundException} with the given message and cause.
   *
   * @param message the exception message
   * @param cause the exception cause
   */
  public NotFoundException(String message, Throwable cause) {
    super(message, cause);
  }
}
