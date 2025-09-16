package com.webforj.spring.security;

import com.webforj.router.security.RouteSecurityContext;
import java.util.Map;
import java.util.Optional;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;

/**
 * Spring Security implementation of {@link RouteSecurityContext}.
 *
 * @author Hyyan Abo Fakher
 * @since 25.10
 */
public class SpringRouteSecurityContext implements RouteSecurityContext {

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isAuthenticated() {
    Authentication auth = getAuthentication();
    return auth != null && auth.isAuthenticated();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Optional<Object> getPrincipal() {
    Authentication auth = getAuthentication();
    return auth != null ? Optional.ofNullable(auth.getPrincipal()) : Optional.empty();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean hasRole(String role) {
    Authentication auth = getAuthentication();
    if (auth == null) {
      return false;
    }

    String roleWithPrefix = role.startsWith("ROLE_") ? role : "ROLE_" + role;
    return auth.getAuthorities().stream().map(GrantedAuthority::getAuthority)
        .anyMatch(authority -> authority.equals(roleWithPrefix));
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean hasAuthority(String authority) {
    Authentication auth = getAuthentication();
    if (auth == null) {
      return false;
    }

    return auth.getAuthorities().stream().map(GrantedAuthority::getAuthority)
        .anyMatch(a -> a.equals(authority));
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Optional<Object> getAttribute(String name) {
    Authentication auth = getAuthentication();
    if (auth != null && auth.getDetails() instanceof Map) {
      @SuppressWarnings("unchecked")
      Map<String, Object> details = (Map<String, Object>) auth.getDetails();

      return Optional.ofNullable(details.get(name));
    }

    return Optional.empty();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void setAttribute(String name, Object value) {
    throw new UnsupportedOperationException(
        "Setting attributes not supported in Spring Security context");
  }

  Authentication getAuthentication() {
    return SecurityContextHolder.getContext().getAuthentication();
  }
}
