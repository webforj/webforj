package com.webforj.bbj.security.model;

import java.time.Instant;
import java.util.Objects;


/**
 * A signed in user together with the token that proves the sign in.
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
public class BBjSession {
  private final BBjPrincipal principal;
  private final String token;
  private final Instant expiresAt;

  /**
   * Creates a session.
   *
   * @param principal the signed in user
   * @param token the token of the sign in
   * @param expiresAt the moment the token expires
   *
   * @throws NullPointerException if any parameter is {@code null}
   * @throws IllegalArgumentException if {@code token} is blank
   */
  public BBjSession(BBjPrincipal principal, String token, Instant expiresAt) {
    Objects.requireNonNull(principal, "principal");
    Objects.requireNonNull(token, "token");
    Objects.requireNonNull(expiresAt, "expiresAt");
    if (token.isBlank()) {
      throw new IllegalArgumentException("token cannot be empty");
    }
    this.principal = principal;
    this.token = token;
    this.expiresAt = expiresAt;
  }

  /**
   * Gets the signed in user.
   *
   * @return the principal
   */
  public BBjPrincipal getPrincipal() {
    return principal;
  }

  /**
   * Gets the token of the sign in.
   *
   * @return the token
   */
  public String getToken() {
    return token;
  }

  /**
   * Gets the moment the token expires.
   *
   * @return the expiry
   */
  public Instant getExpiresAt() {
    return expiresAt;
  }

  /**
   * Gets a text form of the session that leaves out the token.
   *
   * @return the text form
   */
  @Override
  public String toString() {
    return "BBjSession [principal=" + principal + ", expiresAt=" + expiresAt + "]";
  }

}
