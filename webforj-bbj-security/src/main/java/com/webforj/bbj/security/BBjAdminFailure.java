package com.webforj.bbj.security;

import java.io.Serial;
import java.util.Objects;

/**
 * A failed call to the BBj Admin API.
 *
 * <p>
 * The failure has one of a closed set of kinds. It has no cause and no suppressed exceptions, so
 * nothing the original exception said can reach a log through it. The message never contains a
 * password, a token or a two-factor code.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
final class BBjAdminFailure extends RuntimeException {
  @Serial
  private static final long serialVersionUID = 1L;

  private final Kind kind;

  /**
   * Creates a failure.
   *
   * @param kind the kind of the failure
   * @param message the message, free of secrets
   *
   * @throws NullPointerException if an argument is {@code null}
   */
  BBjAdminFailure(Kind kind, String message) {
    super(Objects.requireNonNull(message, "message"), null, false, true);
    this.kind = Objects.requireNonNull(kind, "kind");
  }

  /**
   * Gets the kind of the failure.
   *
   * @return the kind
   */
  Kind getKind() {
    return kind;
  }

  /**
   * The reasons an Admin API call fails.
   */
  enum Kind {
    /**
     * The server refused the username or the password.
     */
    INVALID_CREDENTIALS,

    /**
     * The server requires a two-factor code that was not part of the password.
     */
    TWO_FACTOR_REQUIRED,

    /**
     * The server refused the token because it is expired, invalidated or unknown.
     */
    TOKEN_REJECTED,

    /**
     * The server could not be reached or answered something unknown.
     */
    UNAVAILABLE
  }
}
