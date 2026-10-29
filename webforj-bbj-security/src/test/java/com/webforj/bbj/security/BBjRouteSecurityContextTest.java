package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

import com.webforj.bbj.security.model.BBjPrincipal;
import java.util.Optional;
import java.util.Set;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class BBjRouteSecurityContextTest {

  private final BBjPrincipal principal =
      new BBjPrincipal("alice", Set.of("Admins"), Set.of("READ_PRIVILEGE"));
  private MockedStatic<BBjSecurity> securityMock;

  @BeforeEach
  void setUp() {
    securityMock = mockStatic(BBjSecurity.class);
  }

  @AfterEach
  void tearDown() {
    securityMock.close();
  }

  @Nested
  class WithoutPrincipal {

    private final BBjRouteSecurityContext context = new BBjRouteSecurityContext();

    @BeforeEach
    void setUp() {
      securityMock.when(BBjSecurity::currentUser).thenReturn(Optional.empty());
      securityMock.when(() -> BBjSecurity.hasRole("Admins")).thenReturn(false);
      securityMock.when(() -> BBjSecurity.hasAuthority("READ_PRIVILEGE")).thenReturn(false);
    }

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

    private final BBjRouteSecurityContext context = new BBjRouteSecurityContext();

    @BeforeEach
    void setUp() {
      securityMock.when(BBjSecurity::currentUser).thenReturn(Optional.of(principal));
      securityMock.when(() -> BBjSecurity.hasRole("Admins")).thenReturn(true);
      securityMock.when(() -> BBjSecurity.hasRole("Sales EU")).thenReturn(false);
      securityMock.when(() -> BBjSecurity.hasAuthority("READ_PRIVILEGE")).thenReturn(true);
      securityMock.when(() -> BBjSecurity.hasAuthority("WRITE_PRIVILEGE")).thenReturn(false);
    }

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
