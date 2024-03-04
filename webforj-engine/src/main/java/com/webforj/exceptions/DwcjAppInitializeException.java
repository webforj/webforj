package com.webforj.exceptions;

/**
 * This class is used for reporting errors that occur during DWC application initialization phase.
 *
 * @author Stephan Wald, Hyyan Abo Fakher
 */
public final class DwcjAppInitializeException extends DwcjException {

  private static final String DEFAULT_MESSAGE = "DWC application initialization failed.";

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
  public DwcjAppInitializeException(String message, Throwable cause) {
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
  public DwcjAppInitializeException(Throwable cause) {
    super(DEFAULT_MESSAGE, cause);
  }

  /**
   * Constructs a new exception with the specified detail message. The cause is not initialized, and
   * may subsequently be initialized by a call to {@link #initCause}.
   *
   * @param e the detail message. The detail message is saved for later retrieval by the
   *        {@link #getMessage()} method.
   */
  public DwcjAppInitializeException(String e) {
    super(e);
  }

  /**
   * Constructs a new exception with the specified detail message.
   */
  public DwcjAppInitializeException() {
    super(DEFAULT_MESSAGE);
  }
}
