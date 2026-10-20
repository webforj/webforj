package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.typesafe.config.ConfigFactory;
import com.webforj.router.security.RouteSecurityContext;
import com.webforj.router.security.RouteSecurityEvaluator;
import com.webforj.router.security.evaluator.AnonymousAccessEvaluator;
import com.webforj.router.security.evaluator.AuthenticationRequiredEvaluator;
import com.webforj.router.security.evaluator.DenyAllEvaluator;
import com.webforj.router.security.evaluator.PermitAllEvaluator;
import com.webforj.router.security.evaluator.RolesAllowedEvaluator;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.List;
import org.junit.jupiter.api.Test;

class BBjRouteSecurityManagerTest {
  private final BBjRouteSecurityConfiguration configuration =
      BBjRouteSecurityConfiguration.create(ConfigFactory.empty());
  private final BBjAuthenticationState state = new BBjAuthenticationState();
  private final Instant now = Instant.parse("2026-01-01T00:00:00Z");
  private final BBjRouteSecurityManager manager =
      new BBjRouteSecurityManager(configuration, state, Clock.fixed(now, ZoneOffset.UTC));

  @Test
  void shouldRegisterTheBuiltInEvaluatorsInPriorityOrder() {
    List<Class<?>> types =
        manager.getEvaluators().stream().<Class<?>>map(RouteSecurityEvaluator::getClass).toList();

    assertEquals(List.of(DenyAllEvaluator.class, AnonymousAccessEvaluator.class,
        AuthenticationRequiredEvaluator.class, PermitAllEvaluator.class,
        RolesAllowedEvaluator.class), types);
  }

  @Test
  void shouldReturnTheGivenConfiguration() {
    assertSame(configuration, manager.getConfiguration());
  }

  @Test
  void shouldProvideAnAnonymousSecurityContext() {
    RouteSecurityContext context = manager.getSecurityContext();

    assertFalse(context.isAuthenticated());
    assertTrue(context.getPrincipal().isEmpty());
    assertFalse(context.hasRole("ADMIN"));
    assertFalse(context.hasAuthority("READ"));
    assertTrue(context.getAttribute("name").isEmpty());
  }

  @Test
  void shouldRefuseSecurityContextAttributes() {
    RouteSecurityContext context = manager.getSecurityContext();

    assertThrows(UnsupportedOperationException.class, () -> context.setAttribute("name", "value"));
  }

  @Test
  void shouldProvideAnAuthenticatedSecurityContextFromTheState() {
    state.setAuthentication(new BBjAuthentication("alice", List.of("Admins"), now.plusSeconds(60)));
    RouteSecurityContext context = manager.getSecurityContext();

    assertTrue(context.isAuthenticated());
    assertEquals("alice", context.getPrincipal().orElseThrow());
    assertTrue(context.hasRole("Admins"));
  }

  @Test
  void shouldProvideAnAnonymousSecurityContextOnceTheAuthenticationExpired() {
    state.setAuthentication(new BBjAuthentication("alice", List.of("Admins"), now));
    RouteSecurityContext context = manager.getSecurityContext();

    assertFalse(context.isAuthenticated());
    assertFalse(context.hasRole("Admins"));
  }
}
