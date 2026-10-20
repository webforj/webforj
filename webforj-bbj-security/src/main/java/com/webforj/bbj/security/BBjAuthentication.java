package com.webforj.bbj.security;

import java.time.Instant;
import java.util.Collection;
import java.util.Objects;
import java.util.Set;

/**
 * A signed-in BBj user.
 *
 * <p>
 * Holds the username, the names of the BBj security groups of the user as roles and the instant
 * from which the sign-in no longer counts.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
final class BBjAuthentication {
  private final String userName;
  private final Set<String> roles;
  private final Instant expiresAt;

  /**
   * Creates an authentication.
   *
   * @param userName the BBj username, not blank
   * @param roles the BBj security group names, kept as given
   * @param expiresAt the instant from which the sign-in no longer counts
   *
   * @throws NullPointerException if an argument or a role is {@code null}
   * @throws IllegalArgumentException if the username is blank
   */
  BBjAuthentication(String userName, Collection<String> roles, Instant expiresAt) {
    Objects.requireNonNull(userName, "userName");
    if (userName.isBlank()) {
      throw new IllegalArgumentException("The user name must not be blank");
    }

    this.userName = userName;
    this.roles = Set.copyOf(Objects.requireNonNull(roles, "roles"));
    this.expiresAt = Objects.requireNonNull(expiresAt, "expiresAt");
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
   * Gets the BBj security group names of the user.
   *
   * @return the unmodifiable roles
   */
  Set<String> getRoles() {
    return roles;
  }

  /**
   * Gets the instant from which the sign-in no longer counts.
   *
   * @return the expiry
   */
  Instant getExpiresAt() {
    return expiresAt;
  }
}
