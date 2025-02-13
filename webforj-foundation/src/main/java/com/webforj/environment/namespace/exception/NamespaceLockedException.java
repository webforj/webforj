package com.webforj.environment.namespace.exception;

import com.webforj.exceptions.WebforjException;

/**
 * Exception thrown when an attempt is made to modify a locked namespace variable.
 *
 * @since 24.22
 * @author Stephen Wald
 * @author Hyyan Abo Fakher
 */
public final class NamespaceLockedException extends WebforjException {
  /**
   * Constructs a new exception with {@code null} as its detail message.
   */
  public NamespaceLockedException() {
    super();
  }

  /**
   * Constructs a new exception with the specified detail message.
   *
   * @param message the detail message
   */
  public NamespaceLockedException(String message) {
    super(message);
  }

  /**
   * Constructs a new exception with the specified detail message and cause.
   *
   * @param message the detail message
   * @param cause the cause (a {@code null} value is permitted, and indicates that the cause is
   *        nonexistent or unknown)
   */
  public NamespaceLockedException(String message, Throwable cause) {
    super(message, cause);
  }

  /**
   * Constructs a new exception with the specified cause and a detail message of
   * {@code (cause==null ? null : cause.toString())} (which typically contains the class and detail
   * message of {@code cause}).
   *
   * @param cause the cause (a {@code null} value is permitted, and indicates that the cause is
   *        nonexistent or unknown)
   */
  public NamespaceLockedException(Throwable cause) {
    super(cause);
  }
}
