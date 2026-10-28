package com.webforj.testbed.fixture.security;

import java.util.Set;

/** Holds the current user and roles for security tests. */
public final class TestSecurityContextHolder {
  private static final ThreadLocal<TestSecurityContextHolder> current = new ThreadLocal<>();

  private final String username;
  private final Set<String> roles;

  private TestSecurityContextHolder(String username, Set<String> roles) {
    this.username = username;
    this.roles = roles;
  }

  public static void login(String username, String... roles) {
    current.set(new TestSecurityContextHolder(username, Set.of(roles)));
  }

  public static void logout() {
    current.remove();
  }

  public static boolean isAuthenticated() {
    return current.get() != null;
  }

  public static String getUsername() {
    TestSecurityContextHolder holder = current.get();
    return holder == null ? null : holder.username;
  }

  public static boolean hasRole(String role) {
    TestSecurityContextHolder holder = current.get();
    return holder != null && holder.roles.contains(role);
  }
}
