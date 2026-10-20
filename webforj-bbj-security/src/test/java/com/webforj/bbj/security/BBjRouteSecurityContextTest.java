package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.List;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;

class BBjRouteSecurityContextTest {
  private static final Instant EXPIRES_AT = Instant.parse("2026-01-01T00:00:00Z");

  private final BBjAuthenticationState state = new BBjAuthenticationState();
  private final BBjAuthentication authentication =
      new BBjAuthentication("alice", List.of("Admins", "users"), EXPIRES_AT);

  private static Stream<Arguments> getExpiredInstants() {
    return Stream.of(Arguments.of(EXPIRES_AT), Arguments.of(EXPIRES_AT.plusSeconds(1)));
  }

  private BBjRouteSecurityContext createContext(Instant now) {
    return new BBjRouteSecurityContext(state, Clock.fixed(now, ZoneOffset.UTC));
  }

  @Test
  void shouldBeAnonymousWithoutAnAuthentication() {
    BBjRouteSecurityContext context = createContext(EXPIRES_AT.minusSeconds(60));

    assertFalse(context.isAuthenticated());
    assertTrue(context.getPrincipal().isEmpty());
    assertFalse(context.hasRole("Admins"));
    assertFalse(context.hasAuthority("READ"));
  }

  @Test
  void shouldBeAuthenticatedBeforeTheExpiry() {
    state.setAuthentication(authentication);
    BBjRouteSecurityContext context = createContext(EXPIRES_AT.minusSeconds(1));

    assertTrue(context.isAuthenticated());
  }

  @Test
  void shouldReturnTheUserNameAsPrincipal() {
    state.setAuthentication(authentication);
    BBjRouteSecurityContext context = createContext(EXPIRES_AT.minusSeconds(1));

    assertEquals("alice", context.getPrincipal().orElseThrow());
  }

  @Test
  void shouldMatchAnExactRole() {
    state.setAuthentication(authentication);
    BBjRouteSecurityContext context = createContext(EXPIRES_AT.minusSeconds(1));

    assertTrue(context.hasRole("Admins"));
    assertTrue(context.hasRole("users"));
  }

  @ParameterizedTest
  @ValueSource(strings = {"admins", "ADMINS", "ROLE_Admins", "Admin", "Admins ", ""})
  void shouldNotMatchAnotherRole(String role) {
    state.setAuthentication(authentication);
    BBjRouteSecurityContext context = createContext(EXPIRES_AT.minusSeconds(1));

    assertFalse(context.hasRole(role));
  }

  @Test
  void shouldNeverHaveAnAuthority() {
    state.setAuthentication(authentication);
    BBjRouteSecurityContext context = createContext(EXPIRES_AT.minusSeconds(1));

    assertFalse(context.hasAuthority("Admins"));
    assertFalse(context.hasAuthority("READ"));
  }

  @ParameterizedTest
  @MethodSource("getExpiredInstants")
  void shouldBeAnonymousOnceExpired(Instant now) {
    state.setAuthentication(authentication);
    BBjRouteSecurityContext context = createContext(now);

    assertFalse(context.isAuthenticated());
    assertTrue(context.getPrincipal().isEmpty());
    assertFalse(context.hasRole("Admins"));
    assertFalse(context.hasAuthority("Admins"));
  }

  @Test
  void shouldFollowTheStateAndTheClock() {
    BBjRouteSecurityContext context = createContext(EXPIRES_AT.minusSeconds(1));
    state.setAuthentication(authentication);
    assertTrue(context.isAuthenticated());

    state.clearAuthentication();

    assertFalse(context.isAuthenticated());
  }

  @Test
  void shouldHaveNoAttributes() {
    state.setAuthentication(authentication);
    BBjRouteSecurityContext context = createContext(EXPIRES_AT.minusSeconds(1));

    assertTrue(context.getAttribute("name").isEmpty());
  }

  @Test
  void shouldRefuseSettingAnAttribute() {
    BBjRouteSecurityContext context = createContext(EXPIRES_AT.minusSeconds(1));

    assertThrows(UnsupportedOperationException.class, () -> context.setAttribute("name", "value"));
  }
}
