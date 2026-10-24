package com.webforj.bbj.security;

import java.util.Objects;
import java.util.Optional;

/**
 * Holds the current {@link BBjAuthentication} of one app.
 *
 * <p>
 * The state starts empty.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
final class BBjAuthenticationState {
  private volatile BBjAuthentication authentication;

  /**
   * Gets the current authentication.
   *
   * @return the authentication, or empty when nobody is signed in
   */
  Optional<BBjAuthentication> getAuthentication() {
    return Optional.ofNullable(authentication);
  }

  /**
   * Sets the current authentication.
   *
   * @param authentication the authentication to hold
   *
   * @throws NullPointerException if the authentication is {@code null}
   */
  void setAuthentication(BBjAuthentication authentication) {
    this.authentication = Objects.requireNonNull(authentication, "authentication");
  }

  /**
   * Clears the current authentication.
   */
  void clearAuthentication() {
    this.authentication = null;
  }
}
