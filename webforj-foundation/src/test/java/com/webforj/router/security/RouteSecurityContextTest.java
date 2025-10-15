package com.webforj.router.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class RouteSecurityContextTest {

  private TestSecurityContext context;

  @BeforeEach
  void setUp() {
    context = new TestSecurityContext();
  }

  @Test
  void shouldManageAuthenticationState() {
    assertFalse(context.isAuthenticated());
    assertFalse(context.getPrincipal().isPresent());

    context.setAuthenticated(true);
    context.setPrincipal("testUser");

    assertTrue(context.isAuthenticated());
    assertTrue(context.getPrincipal().isPresent());
    assertEquals("testUser", context.getPrincipal().get());
  }

  @Test
  void shouldCheckRoles() {
    assertFalse(context.hasRole("ADMIN"));
    assertFalse(context.hasRole("USER"));

    context.addRole("USER");
    assertFalse(context.hasRole("ADMIN"));
    assertTrue(context.hasRole("USER"));

    context.addRole("ADMIN");
    assertTrue(context.hasRole("ADMIN"));
    assertTrue(context.hasRole("USER"));
  }

  @Test
  void shouldCheckAuthorities() {
    assertFalse(context.hasAuthority("READ_USERS"));
    assertFalse(context.hasAuthority("WRITE_POSTS"));

    context.addAuthority("READ_USERS");
    assertTrue(context.hasAuthority("READ_USERS"));
    assertFalse(context.hasAuthority("WRITE_POSTS"));

    context.addAuthority("WRITE_POSTS");
    assertTrue(context.hasAuthority("READ_USERS"));
    assertTrue(context.hasAuthority("WRITE_POSTS"));
  }

  @Test
  void shouldManageAttributes() {
    assertFalse(context.getAttribute("key1").isPresent());

    context.setAttribute("key1", "value1");
    assertTrue(context.getAttribute("key1").isPresent());
    assertEquals("value1", context.getAttribute("key1").get());

    context.setAttribute("key2", 42);
    assertEquals(42, context.getAttribute("key2").get());

    context.setAttribute("key1", "updatedValue");
    assertEquals("updatedValue", context.getAttribute("key1").get());
  }

  @Test
  void shouldHandleNullValues() {
    assertFalse(context.hasRole(null));
    assertFalse(context.hasAuthority(null));
    assertFalse(context.getAttribute(null).isPresent());

    context.setAttribute(null, "value");
    assertFalse(context.getAttribute(null).isPresent());

    context.setAttribute("key", null);
    assertFalse(context.getAttribute("key").isPresent());
  }

  private static class TestSecurityContext implements RouteSecurityContext {
    private boolean authenticated = false;
    private Object principal = null;
    private final Map<String, Boolean> roles = new HashMap<>();
    private final Map<String, Boolean> authorities = new HashMap<>();
    private final Map<String, Object> attributes = new HashMap<>();

    @Override
    public boolean isAuthenticated() {
      return authenticated;
    }

    public void setAuthenticated(boolean authenticated) {
      this.authenticated = authenticated;
    }

    @Override
    public Optional<Object> getPrincipal() {
      return Optional.ofNullable(principal);
    }

    public void setPrincipal(Object principal) {
      this.principal = principal;
    }

    @Override
    public boolean hasRole(String role) {
      return role != null && roles.getOrDefault(role, false);
    }

    public void addRole(String role) {
      if (role != null) {
        roles.put(role, true);
      }
    }

    @Override
    public boolean hasAuthority(String authority) {
      return authority != null && authorities.getOrDefault(authority, false);
    }

    public void addAuthority(String authority) {
      if (authority != null) {
        authorities.put(authority, true);
      }
    }

    @Override
    public Optional<Object> getAttribute(String name) {
      if (name == null) {
        return Optional.empty();
      }
      return Optional.ofNullable(attributes.get(name));
    }

    @Override
    public void setAttribute(String name, Object value) {
      if (name != null) {
        attributes.put(name, value);
      }
    }
  }
}
