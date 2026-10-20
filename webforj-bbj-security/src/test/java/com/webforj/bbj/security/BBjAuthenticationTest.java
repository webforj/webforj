package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class BBjAuthenticationTest {
  private static final Instant EXPIRES_AT = Instant.parse("2026-01-01T00:00:00Z");

  @Test
  void shouldExposeTheGivenValues() {
    BBjAuthentication authentication =
        new BBjAuthentication("alice", List.of("Admins", "users"), EXPIRES_AT);

    assertEquals("alice", authentication.getUserName());
    assertEquals(Set.of("Admins", "users"), authentication.getRoles());
    assertEquals(EXPIRES_AT, authentication.getExpiresAt());
  }

  @Test
  void shouldKeepRolesVerbatim() {
    BBjAuthentication authentication =
        new BBjAuthentication("alice", List.of("ROLE_x", " Mixed Case "), EXPIRES_AT);

    assertEquals(Set.of("ROLE_x", " Mixed Case "), authentication.getRoles());
  }

  @Test
  void shouldCopyRolesDefensively() {
    List<String> roles = new ArrayList<>(List.of("Admins"));
    BBjAuthentication authentication = new BBjAuthentication("alice", roles, EXPIRES_AT);

    roles.add("users");

    assertEquals(Set.of("Admins"), authentication.getRoles());
  }

  @Test
  void shouldRefuseChangesToTheRoles() {
    BBjAuthentication authentication =
        new BBjAuthentication("alice", List.of("Admins"), EXPIRES_AT);
    Set<String> roles = authentication.getRoles();

    assertThrows(UnsupportedOperationException.class, () -> roles.add("users"));
  }

  @ParameterizedTest
  @ValueSource(strings = {"", "  ", "\t"})
  void shouldRefuseABlankUserName(String userName) {
    List<String> roles = List.of();

    assertThrows(IllegalArgumentException.class,
        () -> new BBjAuthentication(userName, roles, EXPIRES_AT));
  }

  @Test
  void shouldRefuseANullUserName() {
    List<String> roles = List.of();

    assertThrows(NullPointerException.class, () -> new BBjAuthentication(null, roles, EXPIRES_AT));
  }

  @Test
  void shouldRefuseNullRoles() {
    assertThrows(NullPointerException.class,
        () -> new BBjAuthentication("alice", null, EXPIRES_AT));
  }

  @Test
  void shouldRefuseANullRole() {
    List<String> roles = Arrays.asList("Admins", null);

    assertThrows(NullPointerException.class,
        () -> new BBjAuthentication("alice", roles, EXPIRES_AT));
  }

  @Test
  void shouldRefuseANullExpiry() {
    List<String> roles = List.of();

    assertThrows(NullPointerException.class, () -> new BBjAuthentication("alice", roles, null));
  }
}
