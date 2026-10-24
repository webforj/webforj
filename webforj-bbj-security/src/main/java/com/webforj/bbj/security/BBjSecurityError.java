package com.webforj.bbj.security;

import java.io.Serial;

/**
 * A failure that stops the app start.
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
final class BBjSecurityError extends Error {
  @Serial
  private static final long serialVersionUID = 1L;

  /**
   * Creates an error.
   *
   * @param message the message
   * @param cause the cause
   */
  BBjSecurityError(String message, Throwable cause) {
    super(message, cause);
  }
}
