package com.webforj.bbj.security;

import com.webforj.bbj.security.model.BBjPrincipal;
import com.webforj.router.security.RouteSecurityContext;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Supplier;

/**
 * {@link RouteSecurityContext} of a BBj application.
 *
 * <p>
 * The principal of the current user is provided by a supplier.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
public final class BBjRouteSecurityContext implements RouteSecurityContext {

  BBjRouteSecurityContext() {}

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isAuthenticated() {
    return BBjSecurity.currentUser().isPresent();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Optional<Object> getPrincipal() {
    return BBjSecurity.currentUser().map(Object.class::cast);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean hasRole(String role) {
    return BBjSecurity.hasRole(role);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean hasAuthority(String authority) {
    return BBjSecurity.hasAuthority(authority);
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
    throw new UnsupportedOperationException("setAttribute");
  }

}
