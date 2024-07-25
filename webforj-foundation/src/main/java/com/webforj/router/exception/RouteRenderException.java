package com.webforj.router.exception;

/**
 * Represents an exception that occurs during route rendering.
 *
 * <p>
 * Route rendering exceptions are thrown when an error occurs while rendering a route.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public class RouteRenderException extends RuntimeException {

  /**
   * Constructs a new {@code RouteRenderExpcetion} with the given message.
   *
   * @param message the exception message
   */
  public RouteRenderException(String message) {
    super(message);
  }

  /**
   * Constructs a new {@code RouteRenderExpcetion} with the given message and cause.
   *
   * @param message the exception message
   * @param cause the exception cause
   */
  public RouteRenderException(String message, Throwable cause) {
    super(message, cause);
  }
}
