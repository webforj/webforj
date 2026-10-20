package com.webforj.bbj.security.model;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import org.junit.jupiter.api.Test;

class BBjPrincipalTest {

  @Test
  void shouldExposeUserNameAndRoles() {
    BBjPrincipal principal = new BBjPrincipal("alice", List.of("admin", "staff"));

    assertEquals("alice", principal.getUserName());
    assertEquals(Set.of("admin", "staff"), principal.getRoles());
  }

  @Test
  void shouldNotChangeWhenTheGivenRolesChange() {
    List<String> roles = new ArrayList<>(List.of("admin"));
    BBjPrincipal principal = new BBjPrincipal("alice", roles);

    roles.add("staff");

    assertEquals(Set.of("admin"), principal.getRoles());
  }

  @Test
  void shouldHandOutUnmodifiableRoles() {
    BBjPrincipal principal = new BBjPrincipal("alice", List.of("admin"));
    Set<String> roles = principal.getRoles();

    assertThrows(UnsupportedOperationException.class, () -> roles.add("staff"));
  }

  @Test
  void shouldRejectNullArguments() {
    List<String> roles = List.of();

    assertThrows(NullPointerException.class, () -> new BBjPrincipal(null, roles));
    assertThrows(NullPointerException.class, () -> new BBjPrincipal("alice", null));
  }
}
