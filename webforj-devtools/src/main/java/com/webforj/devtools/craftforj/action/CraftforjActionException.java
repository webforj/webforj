package com.webforj.devtools.craftforj.action;

/**
 * Exception thrown by action handlers to indicate a business logic error.
 *
 * <p>
 * When thrown, the {@link CraftforjActionRegistry} catches this exception and returns an error
 * response to the client with the exception message.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class CraftforjActionException extends RuntimeException {

  /**
   * Creates a new action exception.
   *
   * @param message the error message
   */
  public CraftforjActionException(String message) {
    super(message);
  }

  /**
   * Creates a new action exception.
   *
   * @param message the error message
   * @param cause the underlying cause
   */
  public CraftforjActionException(String message, Throwable cause) {
    super(message, cause);
  }
}
