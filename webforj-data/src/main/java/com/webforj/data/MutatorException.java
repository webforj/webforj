package com.webforj.data;

/**
 * This exception is thrown when there is a problem with resolving the getter or setter methods for
 * a property of a bean.
 *
 * @author Hyyan Abo Fakher
 * @version 24.01
 */
public class MutatorException extends RuntimeException {
  /**
   * Constructs a new MutatorException with the specified detail message.
   *
   * @param message the detail message. The detail message is saved for later retrieval by the
   *        Throwable.getMessage() method.
   */
  public MutatorException(String message) {
    super(message);
  }

  /**
   * Constructs a new MutatorException with the specified detail message and cause.
   *
   * @param message the detail message (which is saved for later retrieval by the
   *        Throwable.getMessage() method).
   * @param cause the cause (which is saved for later retrieval by the Throwable.getCause() method).
   *        (A null value is permitted, and indicates that the cause is nonexistent or unknown.)
   */
  public MutatorException(String message, Throwable cause) {
    super(message, cause);
  }
}
