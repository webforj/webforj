package com.webforj.router.exception;

/**
 * Represents an exception that occurs when a route has no target component.
 *
 * <p>
 * Route has no target exceptions are thrown when a route is found in the route registry but the
 * target component of the route is not found.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public class RouteHasNoTargetException extends RouteRenderException {

  /**
   * Constructs a new {@code RouteHasNoTargetException} with the given message.
   *
   * @param message the exception message
   */
  public RouteHasNoTargetException(String message) {
    super(message);
  }

  /**
   * Constructs a new {@code RouteHasNoTargetException} with the given message and cause.
   *
   * @param message the exception message
   * @param cause the exception cause
   */
  public RouteHasNoTargetException(String message, Throwable cause) {
    super(message, cause);
  }
}
