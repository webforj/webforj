package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.typesafe.config.ConfigFactory;
import com.webforj.router.NavigationContext;
import com.webforj.router.NavigationOptions;
import com.webforj.router.Router;
import com.webforj.router.history.Location;
import com.webforj.router.security.RouteAccessDecision;
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
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedStatic;

class BBjRouteSecurityManagerTest {
  private final BBjRouteSecurityConfiguration configuration =
      BBjRouteSecurityConfiguration.create(ConfigFactory.empty());
  private final BBjAuthenticationState state = new BBjAuthenticationState();
  private final Instant now = Instant.parse("2026-01-01T00:00:00Z");
  private final MemoryRequestedLocationStore store = new MemoryRequestedLocationStore();
  private final BBjRouteSecurityManager manager =
      new BBjRouteSecurityManager(configuration, state, Clock.fixed(now, ZoneOffset.UTC), store);

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

  @Test
  void shouldSaveTheRequestedLocationAndRedirectWhenAuthenticationIsRequired() {
    Router router = mock(Router.class);
    NavigationContext context = mock(NavigationContext.class);
    when(context.getLocation()).thenReturn(new Location("/protected?x=1"));

    try (MockedStatic<Router> mockedRouter = mockStatic(Router.class)) {
      mockedRouter.when(Router::getCurrent).thenReturn(router);

      manager.onAccessDenied(RouteAccessDecision.denyAuthentication(), context);
    }

    assertEquals("/protected?x=1", store.getLocation().orElseThrow().getFullURI());
    ArgumentCaptor<Location> target = ArgumentCaptor.forClass(Location.class);
    ArgumentCaptor<NavigationOptions> options = ArgumentCaptor.forClass(NavigationOptions.class);
    verify(router).navigate(target.capture(), options.capture());
    assertEquals("/login", target.getValue().getFullURI());
    assertEquals(NavigationOptions.NavigationType.REPLACE, options.getValue().getNavigationType());
  }

  @Test
  void shouldKeepTheSavedLocationWhenAccessIsDenied() {
    Router router = mock(Router.class);
    NavigationContext context = mock(NavigationContext.class);
    when(context.getLocation()).thenReturn(new Location("/admin"));
    store.saveLocation(new Location("/protected"));

    try (MockedStatic<Router> mockedRouter = mockStatic(Router.class)) {
      mockedRouter.when(Router::getCurrent).thenReturn(router);

      manager.onAccessDenied(RouteAccessDecision.deny("no role"), context);
    }

    assertEquals("/protected", store.getLocation().orElseThrow().getFullURI());
    verify(router).navigate(any(Location.class), any(NavigationOptions.class));
  }

  @Test
  void shouldSaveNothingWhenTheContextHasNoLocation() {
    NavigationContext context = mock(NavigationContext.class);

    try (MockedStatic<Router> ignored = mockStatic(Router.class)) {
      manager.onAccessDenied(RouteAccessDecision.denyAuthentication(), context);
      manager.onAccessDenied(RouteAccessDecision.denyAuthentication(), null);
    }

    assertTrue(store.getLocation().isEmpty());
  }

  @Test
  void shouldNotNavigateWithoutRouter() {
    NavigationContext context = mock(NavigationContext.class);
    when(context.getLocation()).thenReturn(new Location("/protected"));

    try (MockedStatic<Router> mockedRouter = mockStatic(Router.class)) {
      mockedRouter.when(Router::getCurrent).thenReturn(null);

      manager.onAccessDenied(RouteAccessDecision.denyAuthentication(), context);
    }

    assertEquals("/protected", store.getLocation().orElseThrow().getFullURI());
  }

  @Test
  void shouldReadThePreAuthenticationLocationFromTheStore() {
    store.saveLocation(new Location("/protected"));

    assertEquals("/protected", manager.getPreAuthenticationLocation().orElseThrow().getFullURI());
    assertTrue(store.getLocation().isPresent());
  }

  @Test
  void shouldClearThePreAuthenticationLocationInTheStore() {
    store.saveLocation(new Location("/protected"));

    manager.clearPreAuthenticationLocation();

    assertTrue(store.getLocation().isEmpty());
  }

  @Test
  void shouldConsumeThePreAuthenticationLocation() {
    store.saveLocation(new Location("/protected"));

    assertEquals("/protected",
        manager.consumePreAuthenticationLocation().orElseThrow().getFullURI());
    assertTrue(manager.consumePreAuthenticationLocation().isEmpty());
    assertTrue(store.getLocation().isEmpty());
  }

  private static final class MemoryRequestedLocationStore implements BBjRequestedLocationStore {
    private Location location;

    Optional<Location> getLocation() {
      return Optional.ofNullable(location);
    }

    @Override
    public void saveLocation(Location location) {
      this.location = location;
    }

    @Override
    public Optional<Location> readLocation() {
      return Optional.ofNullable(location);
    }

    @Override
    public void clearLocation() {
      location = null;
    }
  }
}
