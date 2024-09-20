package com.webforj.conceiver.exception;

import com.webforj.exceptions.WebforjRuntimeException;

/**
 * Represents an exception that occurs when creating an instance of a class using a
 * {@link Conceiver}.
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public class ConceiverException extends WebforjRuntimeException {

  /**
   * Constructs a new {@code ConceiverException} with the given message.
   *
   * @param message the exception message
   */
  public ConceiverException(String message) {
    super(message);
  }

  /**
   * Constructs a new {@code ConceiverException} with the given message and cause.
   *
   * @param message the exception message
   * @param cause the exception cause
   */
  public ConceiverException(String message, Throwable cause) {
    super(message, cause);
  }
}
