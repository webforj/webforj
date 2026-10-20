package com.webforj.bbj.security;

import com.webforj.router.security.RouteSecurityContext;
import java.time.Clock;
import java.util.Optional;

/**
 * Security context of an app running in BBjServices.
 *
 * <p>
 * The context is authenticated while the state holds an authentication that has not expired. An
 * expired authentication behaves like no authentication.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
final class BBjRouteSecurityContext implements RouteSecurityContext {
  private final BBjAuthenticationState state;
  private final Clock clock;

  BBjRouteSecurityContext(BBjAuthenticationState state, Clock clock) {
    this.state = state;
    this.clock = clock;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isAuthenticated() {
    return getActiveAuthentication().isPresent();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Optional<Object> getPrincipal() {
    return getActiveAuthentication().map(BBjAuthentication::getUserName);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean hasRole(String role) {
    return getActiveAuthentication().map(authentication -> authentication.getRoles().contains(role))
        .orElse(false);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean hasAuthority(String authority) {
    return false;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Optional<Object> getAttribute(String name) {
    return Optional.empty();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void setAttribute(String name, Object value) {
    throw new UnsupportedOperationException(
        "Setting attributes is not supported in the BBj security context");
  }

  Optional<BBjAuthentication> getActiveAuthentication() {
    return state.getAuthentication()
        .filter(authentication -> clock.instant().isBefore(authentication.getExpiresAt()));
  }
}
