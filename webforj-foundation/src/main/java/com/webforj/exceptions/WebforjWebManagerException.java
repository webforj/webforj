package com.webforj.exceptions;

/**
 * This class is used for reporting errors that occur when trying to access the web manager API
 * without being available.
 *
 * @author Hyyan Abo Fakher
 * @since 24.20
 */
public class WebforjWebManagerException extends WebforjRuntimeException {

  /**
   * Constructs a new exception with the specified detail message. The cause is not initialized, and
   * may subsequently be initialized by a call to {@link #initCause}.
   *
   * @param message the detail message. The detail message is saved for later retrieval by the
   *        {@link #getMessage()} method.
   */
  public WebforjWebManagerException(String message) {
    super(message);
  }

  /**
   * Constructs a new exception with the specified detail message and cause.
   *
   * <p>
   * Note that the detail message associated with {@code cause} is <i>not</i> automatically
   * incorporated in this exception's detail message.
   * </p>
   *
   * @param message the detail message (which is saved for later retrieval by the
   *        {@link #getMessage()} method).
   * @param cause the cause (which is saved for later retrieval by the {@link #getCause()} method).
   *        (A {@code null} value is permitted, and indicates that the cause is nonexistent or
   *        unknown.)
   */
  public WebforjWebManagerException(String message, Throwable cause) {
    super(message, cause);
  }

  /**
   * Constructs a new exception with the specified cause and a detail message of
   * {@code (cause==null ? null : cause.toString())} (which typically contains the class and detail
   * message of {@code cause}). This constructor is useful for exceptions that are little more than
   * wrappers for other throwables.
   *
   * @param cause the cause (which is saved for later retrieval by the {@link #getCause()} method).
   *        (A {@code null} value is permitted, and indicates that the cause is nonexistent or
   *        unknown.)
   */
  public WebforjWebManagerException(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs a new DWC exception based on the passed exception.
   *
   * @param e the exception to be wrapped
   */
  public WebforjWebManagerException(Exception e) {
    super(e);
  }

  /**
   * Constructs a new exception with {@code null} as its detail message. The cause is not
   * initialized, and may subsequently be initialized by a call to {@link #initCause}.
   */
  public WebforjWebManagerException() {}
}
