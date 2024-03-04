package com.webforj.exceptions;

/**
 * Exception thrown when the API user is trying to access a restricted API.
 *
 * @author Hyyan Abo Fakher
 * @version 23.02
 */
public class DwcjRestrictedAccessException extends DwcjRuntimeException {

  /**
   * Construct a new DwcjRestrictedAttribute exception.
   *
   * @param message the exception message
   */
  public DwcjRestrictedAccessException(String message) {
    super(message);
  }

  /**
   * Construct a new DwcjRestrictedAttribute exception.
   *
   * @param message the exception message
   * @param cause the cause of the exception
   */
  public DwcjRestrictedAccessException(String message, Throwable cause) {
    super(message, cause);
  }
}
