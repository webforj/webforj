package com.webforj.bbj.security;

import java.time.Instant;
import java.util.Objects;

/**
 * The result of a BBj login, the username the server knows, the login token and the instant from
 * which the token no longer counts.
 *
 * <p>
 * The token is never part of {@link #toString()}.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
final class BBjLoginSession {
  private final String userName;
  private final String token;
  private final Instant expiresAt;

  /**
   * Creates a login session.
   *
   * @param userName the BBj username, not blank
   * @param token the login token, not blank
   * @param expiresAt the instant from which the token no longer counts
   *
   * @throws NullPointerException if an argument is {@code null}
   * @throws IllegalArgumentException if the username or the token is blank
   */
  BBjLoginSession(String userName, String token, Instant expiresAt) {
    Objects.requireNonNull(userName, "userName");
    Objects.requireNonNull(token, "token");
    if (userName.isBlank()) {
      throw new IllegalArgumentException("The user name must not be blank");
    }

    if (token.isBlank()) {
      throw new IllegalArgumentException("The token must not be blank");
    }

    this.userName = userName;
    this.token = token;
    this.expiresAt = Objects.requireNonNull(expiresAt, "expiresAt");
  }

  /**
   * Describes the session without the token.
   *
   * @return the username and the expiry
   */
  @Override
  public String toString() {
    return "BBjLoginSession{userName=" + userName + ", expiresAt=" + expiresAt + "}";
  }

  /**
   * Gets the BBj username.
   *
   * @return the username
   */
  String getUserName() {
    return userName;
  }

  /**
   * Gets the login token.
   *
   * @return the token
   */
  String getToken() {
    return token;
  }

  /**
   * Gets the instant from which the token no longer counts.
   *
   * @return the expiry
   */
  Instant getExpiresAt() {
    return expiresAt;
  }
}
