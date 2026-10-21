package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.bbj.security.model.BBjPrincipal;
import com.webforj.environment.ObjectTable;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.List;
import java.util.Set;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

class BBjSecurityTest {
  private static final String KEY = BBjRouteSecurityContext.class.getName();
  private static final Instant NOW = Instant.parse("2026-01-01T12:00:00Z");
  private final Clock clock = Clock.fixed(NOW, ZoneOffset.UTC);
  private final BBjAuthenticationState state = new BBjAuthenticationState();
  private MockedStatic<ObjectTable> mockedObjectTable;

  @BeforeEach
  void setUp() {
    mockedObjectTable = Mockito.mockStatic(ObjectTable.class);
  }

  @AfterEach
  void tearDown() {
    mockedObjectTable.close();
  }

  private void addContext() {
    mockedObjectTable.when(() -> ObjectTable.contains(KEY)).thenReturn(true);
    mockedObjectTable.when(() -> ObjectTable.get(KEY))
        .thenReturn(new BBjRouteSecurityContext(state, clock));
  }

  private void setAuthentication(Instant expiresAt) {
    state.setAuthentication(new BBjAuthentication("alice", List.of("admin"), expiresAt));
  }

  @Nested
  class Anonymous {

    @BeforeEach
    void setUp() {
      addContext();
    }

    @Test
    void shouldHaveNoCurrentUser() {
      assertTrue(BBjSecurity.currentUser().isEmpty());
    }

    @Test
    void shouldHaveNoRole() {
      assertFalse(BBjSecurity.hasRole("admin"));
    }

    @Test
    void shouldFailToRequireAuthentication() {
      SecurityException exception = assertThrows(SecurityException.class, BBjSecurity::require);

      assertEquals("Authentication is required, sign in and try again", exception.getMessage());
    }

    @Test
    void shouldFailToRequireRole() {
      SecurityException exception =
          assertThrows(SecurityException.class, () -> BBjSecurity.require("admin"));

      assertEquals("Authentication is required, sign in and try again", exception.getMessage());
    }
  }

  @Nested
  class Authenticated {

    @BeforeEach
    void setUp() {
      addContext();
      setAuthentication(NOW.plusSeconds(60));
    }

    @Test
    void shouldReturnTheCurrentUser() {
      BBjPrincipal principal = BBjSecurity.currentUser().orElseThrow();

      assertEquals("alice", principal.getUserName());
      assertEquals(Set.of("admin"), principal.getRoles());
    }

    @Test
    void shouldHaveTheRoleOfTheUserOnly() {
      assertTrue(BBjSecurity.hasRole("admin"));
      assertFalse(BBjSecurity.hasRole("staff"));
    }

    @Test
    void shouldRejectNullRole() {
      assertFalse(BBjSecurity.hasRole(null));
    }

    @Test
    void shouldReturnThePrincipalWhenAuthenticationIsRequired() {
      assertEquals("alice", BBjSecurity.require().getUserName());
    }

    @Test
    void shouldRejectNullRequiredRole() {
      assertThrows(NullPointerException.class, () -> BBjSecurity.require(null));
    }

    @Test
    void shouldReturnThePrincipalWhenTheRoleIsRequired() {
      assertEquals("alice", BBjSecurity.require("admin").getUserName());
    }

    @Test
    void shouldFailWhenTheRequiredRoleIsMissing() {
      SecurityException exception =
          assertThrows(SecurityException.class, () -> BBjSecurity.require("staff"));

      assertEquals("Access denied, the role staff is required", exception.getMessage());
    }

    @Test
    void shouldNeverHavePermissionYet() {
      assertFalse(BBjSecurity.hasPermission("admin"));
    }
  }

  @Nested
  class Expired {

    @BeforeEach
    void setUp() {
      addContext();
      setAuthentication(NOW);
    }

    @Test
    void shouldBehaveLikeAnonymous() {
      assertTrue(BBjSecurity.currentUser().isEmpty());
      assertFalse(BBjSecurity.hasRole("admin"));
      assertThrows(SecurityException.class, BBjSecurity::require);
    }
  }

  @Nested
  class WithoutContext {

    @Test
    void shouldBehaveLikeAnonymousWhenNothingIsRegistered() {
      mockedObjectTable.when(() -> ObjectTable.contains(KEY)).thenReturn(false);

      assertTrue(BBjSecurity.currentUser().isEmpty());
      assertFalse(BBjSecurity.hasRole("admin"));
      assertFalse(BBjSecurity.hasPermission("admin"));
      assertThrows(SecurityException.class, BBjSecurity::require);
      assertThrows(SecurityException.class, () -> BBjSecurity.require("admin"));
    }
  }
}
