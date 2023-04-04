package org.dwcj.exceptions;

/**
 * This class is used for reporting exceptions when DWC control is destroyed
 *
 * @author Hyyan Abo Fakher
 */
public class DwcjComponentDestroyed extends DwcjRuntimeException {
  /**
   * Constructs a new exception with the specified detail message. The cause is not initialized, and
   * may subsequently be initialized by a call to {@link #initCause}.
   *
   * @param message the detail message. The detail message is saved for later retrieval by the
   *        {@link #getMessage()} method.
   */
  public DwcjComponentDestroyed(String message) {
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
  public DwcjComponentDestroyed(String message, Throwable cause) {
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
  public DwcjComponentDestroyed(Throwable cause) {
    super(cause);
  }

  /**
   * Constructs a new DWC exception based on the passed exception.
   *
   * @param e the exception to be wrapped
   */
  public DwcjComponentDestroyed(Exception e) {
    super(e);
  }

  /**
   * Constructs a new exception with {@code null} as its detail message. The cause is not
   * initialized, and may subsequently be initialized by a call to {@link #initCause}.
   */
  public DwcjComponentDestroyed() {}
}
