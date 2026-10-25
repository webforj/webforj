package com.webforj.bbj.security;

/**
 * BBjAdminFailure
 */
public final class BBjAdminFailure extends RuntimeException {
  private final Kind kind;

  public BBjAdminFailure(Kind kind, String message) {
    super(message);
    this.kind = kind;
  }

  public Kind getKind() {
    return kind;
  }

  public enum Kind {
    INVALID_CREDENTIALS, TWO_FACTOR_REQUIRED, TOKEN_REJECTED, UNAVAILABLE
  }
}
