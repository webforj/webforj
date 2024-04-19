package com.webforj.data.transformation;

/**
 * This exception is thrown when there is a problem with transforming a value between the model and
 * the component presentation.
 *
 * @since 24.01
 * @version 24.01
 */
public class TransformationException extends RuntimeException {
  /**
   * Constructs a new TransformationException with the specified detail message.
   *
   * @param message the detail message. The detail message is saved for later retrieval by the
   *        Throwable.getMessage() method.
   */
  public TransformationException(String message) {
    super(message);
  }

  /**
   * Constructs a new TransformationException with the specified detail message and cause.
   *
   * @param message the detail message (which is saved for later retrieval by the
   *        Throwable.getMessage() method).
   * @param cause the cause (which is saved for later retrieval by the Throwable.getCause() method).
   *        (A null value is permitted, and indicates that the cause is nonexistent or unknown.)
   */
  public TransformationException(String message, Throwable cause) {
    super(message, cause);
  }

  /**
   * Constructs a new TransformationException with the specified detail message and cause.
   *
   * @param cause the cause (which is saved for later retrieval by the Throwable.getCause() method).
   *        (A null value is permitted, and indicates that the cause is nonexistent or unknown.)
   */
  public TransformationException(Throwable cause) {
    super(cause);
  }
}
