package com.webforj.devtools.craftforj.inspector.source;

/**
 * Exception thrown when source code modification fails.
 *
 * <p>
 * This exception is used to indicate failures during source code parsing, modification, or
 * generation. Common causes include: source file not found, parse errors, unsupported value types,
 * or complex expressions that cannot be modified.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class SourceModificationException extends RuntimeException {

  /**
   * Creates a new exception with the given message.
   *
   * @param message the error message
   */
  public SourceModificationException(String message) {
    super(message);
  }

  /**
   * Creates a new exception with the given message and cause.
   *
   * @param message the error message
   * @param cause the underlying cause
   */
  public SourceModificationException(String message, Throwable cause) {
    super(message, cause);
  }
}
