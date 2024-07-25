package com.webforj.router.exception;

/**
 * Represents an exception that occurs when a route is not found.
 *
 * <p>
 * Route not found exceptions are thrown when a route is not found in the route registry or when the
 * target component of the route is not found.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public class RouteNotFoundException extends RouteRenderException {

  /**
   * Constructs a new {@code RouteNotFoundException} with the given message.
   *
   * @param message the exception message
   */
  public RouteNotFoundException(String message) {
    super(message);
  }

  /**
   * Constructs a new {@code RouteNotFoundException} with the given message and cause.
   *
   * @param message the exception message
   * @param cause the exception cause
   */
  public RouteNotFoundException(String message, Throwable cause) {
    super(message, cause);
  }
}
