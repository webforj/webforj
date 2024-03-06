package com.webforj.exceptions;

/**
 * Exception thrown when the API user is trying to access a restricted API.
 *
 * @author Hyyan Abo Fakher
 * @version 23.02
 */
public class WebforjRestrictedAccessException extends WebforjRuntimeException {

  /**
   * Construct a new DwcjRestrictedAttribute exception.
   *
   * @param message the exception message
   */
  public WebforjRestrictedAccessException(String message) {
    super(message);
  }

  /**
   * Construct a new DwcjRestrictedAttribute exception.
   *
   * @param message the exception message
   * @param cause the cause of the exception
   */
  public WebforjRestrictedAccessException(String message, Throwable cause) {
    super(message, cause);
  }
}
