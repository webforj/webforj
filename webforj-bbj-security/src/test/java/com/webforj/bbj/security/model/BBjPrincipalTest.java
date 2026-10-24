package com.webforj.bbj.security.model;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Set;
import org.junit.jupiter.api.Test;

class BBjPrincipalTest {

  @Test
  void shouldThrowForNullArguments() {
    assertThrows(NullPointerException.class, () -> new BBjPrincipal(null, Set.of(), Set.of()));
    assertThrows(NullPointerException.class, () -> new BBjPrincipal("alice", null, Set.of()));
    assertThrows(NullPointerException.class, () -> new BBjPrincipal("alice", Set.of(), null));
  }

  void shouldThrowForBlankUsername() {
    assertThrows(IllegalArgumentException.class, () -> new BBjPrincipal("", Set.of(), Set.of()));
    assertThrows(IllegalArgumentException.class, () -> new BBjPrincipal("   ", Set.of(), Set.of()));
    assertThrows(IllegalArgumentException.class, () -> new BBjPrincipal(" ", Set.of(), Set.of()));
  }

  @Test
  void shouldExposeUserNameRolesAndAuthorities() {
    Set<String> roles = Set.of("Admins", "Sales EU");
    Set<String> authorities = Set.of("READ", "WRITE");

    BBjPrincipal principal = new BBjPrincipal("alice", roles, authorities);

    assertEquals("alice", principal.getUserName());
    assertEquals(roles, principal.getRoles());
    assertEquals(authorities, principal.getAuthorities());
  }

  @Test
  void shouldAllowEmptyRolesAndAuthorities() {
    BBjPrincipal principal = new BBjPrincipal("bob", Set.of(), Set.of());

    assertTrue(principal.getRoles().isEmpty());
    assertTrue(principal.getAuthorities().isEmpty());
  }
}
