package com.webforj.bbj.security;

import java.util.Objects;
import java.util.Optional;
import java.util.function.Supplier;

import com.webforj.bbj.security.model.BBjPrincipal;
import com.webforj.router.security.RouteSecurityContext;

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
public class BBjRouteSecurityContext implements RouteSecurityContext {
  private final Supplier<Optional<BBjPrincipal>> principalSupplier;

  /**
   * Creates a context that reads the principal from the given supplier.
   *
   * @param principalSupplier the supplier of the current principal, empty when no user is
   *        authenticated
   *
   * @throws NullPointerException if {@code principalSupplier} is null
   */
  public BBjRouteSecurityContext(Supplier<Optional<BBjPrincipal>> principalSupplier) {
    this.principalSupplier = Objects.requireNonNull(principalSupplier, "principalSupplier");
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isAuthenticated() {
    return principalSupplier.get().isPresent();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Optional<Object> getPrincipal() {
    return principalSupplier.get().map(Object.class::cast);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean hasRole(String role) {
    return principalSupplier.get().map(it -> it.getRoles().contains(role)).orElse(false);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean hasAuthority(String authority) {
    return principalSupplier.get().map(it -> it.getAuthorities().contains(authority)).orElse(false);
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
  public void setAttribute(String name, Object value) {}

}
