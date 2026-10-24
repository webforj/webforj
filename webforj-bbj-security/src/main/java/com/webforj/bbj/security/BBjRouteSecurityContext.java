package com.webforj.bbj.security;

import java.util.Objects;
import java.util.Optional;
import java.util.function.Supplier;

import com.webforj.bbj.security.model.BBjPrincipal;
import com.webforj.router.security.RouteSecurityContext;

/**
 * BBjRouteSecurityContext
 */
public class BBjRouteSecurityContext implements RouteSecurityContext {
  private final Supplier<Optional<BBjPrincipal>> principalSupplier;

  public BBjRouteSecurityContext(Supplier<Optional<Object>> principalSupplier) {
    this.principalSupplier = principalSupplier;
  public BBjRouteSecurityContext(Supplier<Optional<BBjPrincipal>> principalSupplier) {
    this.principalSupplier = Objects.requireNonNull(principalSupplier, "principalSupplier");
  }

  @Override
  public boolean isAuthenticated() {
    return principalSupplier.get().isPresent();
  }

  @Override
  public Optional<Object> getPrincipal() {
    return principalSupplier.get().map(Object.class::cast);
  }

  @Override
  public boolean hasRole(String role) {
    return principalSupplier.get().map(it -> it.getRoles().contains(role)).orElse(false);
  }

  @Override
  public boolean hasAuthority(String authority) {
    return principalSupplier.get().map(it -> it.getAuthorities().contains(authority)).orElse(false);
  }

  @Override
  public Optional<Object> getAttribute(String name) {
    return Optional.empty();
  }

  @Override
  public void setAttribute(String name, Object value) {}

}
