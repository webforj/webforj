package com.webforj.testbed.fixture.security;

import com.webforj.router.security.RouteSecurityContext;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

/** Provides the route security context for tests. */
public class TestRouteSecurityContext implements RouteSecurityContext {
  private final Map<String, Object> attributes = new ConcurrentHashMap<>();

  @Override
  public boolean isAuthenticated() {
    return TestSecurityContextHolder.isAuthenticated();
  }

  @Override
  public Optional<Object> getPrincipal() {
    return Optional.ofNullable(TestSecurityContextHolder.getUsername());
  }

  @Override
  public boolean hasRole(String role) {
    return TestSecurityContextHolder.hasRole(role);
  }

  @Override
  public boolean hasAuthority(String authority) {
    return TestSecurityContextHolder.hasRole(authority);
  }

  @Override
  public Optional<Object> getAttribute(String name) {
    return Optional.ofNullable(attributes.get(name));
  }

  @Override
  public void setAttribute(String name, Object value) {
    attributes.put(name, value);
  }
}
