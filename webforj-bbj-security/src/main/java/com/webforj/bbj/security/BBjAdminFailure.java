package com.webforj.bbj.security;

/**
 * Signals that a call to the BBj Admin API did not succeed.
 *
 * <p>
 * The kind of the failure tells the cause.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
public final class BBjAdminFailure extends RuntimeException {
  private final Kind kind;

  /**
   * Creates a failure.
   *
   * @param kind the kind of the failure
   * @param message the message of the failure
   */
  public BBjAdminFailure(Kind kind, String message) {
    super(message);
    this.kind = kind;
  }

  /**
   * Gets the kind of the failure.
   *
   * @return the kind
   */
  public Kind getKind() {
    return kind;
  }

  /**
   * The cause of a failure.
   *
   * @since 26.03
   */
  public enum Kind {
    /** The user name or the password was refused. */
    INVALID_CREDENTIALS,

    /** The 2 factor code was refused. */
    INVALID_CODE,

    /** The account needs a 2 factor code that was not given. */
    TWO_FACTOR_REQUIRED,

    /** The server refused an issued token as expired, unverifiable or from a failed login. */
    TOKEN_REJECTED,

    /** The Admin API could not be reached or could not complete the call. */
    UNAVAILABLE
  }
}
