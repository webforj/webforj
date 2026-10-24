package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.bbj.security.model.BBjPrincipal;
import java.util.Optional;
import java.util.Set;
import java.util.function.Supplier;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class BBjRouteSecurityContextTest {

  private final BBjPrincipal principal =
      new BBjPrincipal("alice", Set.of("Admins"), Set.of("READ_PRIVILEGE"));

  @Test
  void shouldRejectNullSupplier() {
    Supplier<Optional<BBjPrincipal>> missing = null;

    assertThrows(NullPointerException.class, () -> new BBjRouteSecurityContext(missing));
  }

  @Nested
  class WithoutPrincipal {

    private final BBjRouteSecurityContext context = new BBjRouteSecurityContext(Optional::empty);

    @Test
    void shouldNotBeAuthenticated() {
      assertFalse(context.isAuthenticated());
    }

    @Test
    void shouldReturnEmptyPrincipal() {
      assertFalse(context.getPrincipal().isPresent());
    }

    @Test
    void shouldNotHaveRole() {
      assertFalse(context.hasRole("Admins"));
    }

    @Test
    void shouldNotHaveAuthority() {
      assertFalse(context.hasAuthority("READ_PRIVILEGE"));
    }

    @Test
    void shouldReturnEmptyAttribute() {
      assertFalse(context.getAttribute("sessionId").isPresent());
    }
  }

  @Nested
  class WithPrincipal {

    private final BBjRouteSecurityContext context =
        new BBjRouteSecurityContext(() -> Optional.of(principal));

    @Test
    void shouldBeAuthenticated() {
      assertTrue(context.isAuthenticated());
    }

    @Test
    void shouldReturnPrincipal() {
      assertSame(principal, context.getPrincipal().orElseThrow());
    }

    @Test
    void shouldHaveRoleOfPrincipal() {
      assertTrue(context.hasRole("Admins"));
      assertFalse(context.hasRole("Sales EU"));
    }

    @Test
    void shouldHaveAuthorityOfPrincipal() {
      assertTrue(context.hasAuthority("READ_PRIVILEGE"));
      assertFalse(context.hasAuthority("WRITE_PRIVILEGE"));
    }
  }
}
