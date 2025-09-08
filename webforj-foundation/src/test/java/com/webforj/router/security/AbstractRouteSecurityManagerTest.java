package com.webforj.router.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.router.NavigationContext;
import com.webforj.router.Router;
import com.webforj.router.annotation.Route;
import com.webforj.router.history.Location;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class AbstractRouteSecurityManagerTest {

  private TestSecurityManager securityManager;
  private RouteSecurityContext securityContext;
  private RouteSecurityConfiguration configuration;
  private NavigationContext navigationContext;

  @BeforeEach
  void setUp() {
    securityContext = mock(RouteSecurityContext.class);
    configuration = mock(RouteSecurityConfiguration.class);
    navigationContext = mock(NavigationContext.class);
    securityManager = new TestSecurityManager(securityContext, configuration);

    when(configuration.isEnabled()).thenReturn(true);
    when(configuration.isSecureByDefault()).thenReturn(true);
  }

  @Test
  void shouldRegisterEvaluatorWithPriority() {
    RouteSecurityEvaluator evaluator = mock(RouteSecurityEvaluator.class);

    securityManager.registerEvaluator(evaluator, 10.0);

    List<RouteSecurityEvaluator> evaluators = securityManager.getEvaluators();
    assertEquals(1, evaluators.size());
    assertEquals(evaluator, evaluators.get(0));
  }

  @Test
  void shouldThrowForInvalidPriority() {
    RouteSecurityEvaluator evaluator = mock(RouteSecurityEvaluator.class);

    assertThrows(IllegalArgumentException.class,
        () -> securityManager.registerEvaluator(evaluator, 0.0));
    assertThrows(IllegalArgumentException.class,
        () -> securityManager.registerEvaluator(evaluator, -1.0));
  }

  @Test
  void shouldSortEvaluatorsByPriority() {
    RouteSecurityEvaluator evaluator1 = mock(RouteSecurityEvaluator.class);
    RouteSecurityEvaluator evaluator2 = mock(RouteSecurityEvaluator.class);
    RouteSecurityEvaluator evaluator3 = mock(RouteSecurityEvaluator.class);

    securityManager.registerEvaluator(evaluator2, 20.0);
    securityManager.registerEvaluator(evaluator3, 30.0);
    securityManager.registerEvaluator(evaluator1, 10.0);

    List<RouteSecurityEvaluator> evaluators = securityManager.getEvaluators();
    assertEquals(3, evaluators.size());
    assertEquals(evaluator1, evaluators.get(0));
    assertEquals(evaluator2, evaluators.get(1));
    assertEquals(evaluator3, evaluators.get(2));
  }

  @Test
  void shouldUnregisterEvaluator() {
    RouteSecurityEvaluator evaluator1 = mock(RouteSecurityEvaluator.class);
    RouteSecurityEvaluator evaluator2 = mock(RouteSecurityEvaluator.class);

    securityManager.registerEvaluator(evaluator1, 10.0);
    securityManager.registerEvaluator(evaluator2, 20.0);
    assertEquals(2, securityManager.getEvaluators().size());

    securityManager.unregisterEvaluator(evaluator1);
    List<RouteSecurityEvaluator> evaluators = securityManager.getEvaluators();
    assertEquals(1, evaluators.size());
    assertEquals(evaluator2, evaluators.get(0));
  }

  @Test
  void shouldGrantAccessWhenSecurityDisabled() {
    when(configuration.isEnabled()).thenReturn(false);

    RouteAccessDecision decision = securityManager.evaluate(TestRoute.class, navigationContext);

    assertTrue(decision.isGranted());
  }

  @Test
  void shouldEvaluateThroughChain() {
    when(configuration.isEnabled()).thenReturn(true);
    when(securityContext.isAuthenticated()).thenReturn(true);

    RouteSecurityEvaluator evaluator = mock(RouteSecurityEvaluator.class);
    when(evaluator.supports(TestRoute.class)).thenReturn(true);
    when(evaluator.evaluate(eq(TestRoute.class), eq(navigationContext), eq(securityContext),
        any(SecurityEvaluatorChain.class))).thenReturn(RouteAccessDecision.grant());

    securityManager.registerEvaluator(evaluator, 10.0);

    RouteAccessDecision decision = securityManager.evaluate(TestRoute.class, navigationContext);

    assertTrue(decision.isGranted());
    verify(evaluator).evaluate(eq(TestRoute.class), eq(navigationContext), eq(securityContext),
        any(SecurityEvaluatorChain.class));
  }

  @Test
  void shouldApplySecureByDefaultWhenNoEvaluatorHandles() {
    when(configuration.isSecureByDefault()).thenReturn(true);
    when(securityContext.isAuthenticated()).thenReturn(false);

    RouteAccessDecision decision = securityManager.evaluate(TestRoute.class, navigationContext);

    assertTrue(decision.isDenied());
    assertTrue(decision.isAuthenticationRequired());
  }

  @Test
  void shouldGrantWhenNotSecureByDefaultAndNoEvaluatorHandles() {
    when(configuration.isSecureByDefault()).thenReturn(false);
    when(securityContext.isAuthenticated()).thenReturn(false);

    RouteAccessDecision decision = securityManager.evaluate(TestRoute.class, navigationContext);

    assertTrue(decision.isGranted());
  }

  @Test
  void shouldHandleAuthenticationRequiredDenial() {
    RouteAccessDecision decision = RouteAccessDecision.denyAuthentication();
    Location authLocation = new Location("/login");
    when(configuration.getAuthenticationLocation()).thenReturn(Optional.of(authLocation));

    try (MockedStatic<Router> routerMock = mockStatic(Router.class)) {
      Router router = mock(Router.class);
      routerMock.when(Router::getCurrent).thenReturn(router);

      securityManager.onAccessDenied(decision, navigationContext);

      verify(router).navigate(authLocation);
    }
  }

  @Test
  void shouldHandleInsufficientPermissionsDenial() {
    RouteAccessDecision decision = RouteAccessDecision.denyPermissions("No access");
    Location forbiddenLocation = new Location("/forbidden");
    when(configuration.getInsufficientPermissionsLocation())
        .thenReturn(Optional.of(forbiddenLocation));

    try (MockedStatic<Router> routerMock = mockStatic(Router.class)) {
      Router router = mock(Router.class);
      routerMock.when(Router::getCurrent).thenReturn(router);

      securityManager.onAccessDenied(decision, navigationContext);

      verify(router).navigate(forbiddenLocation);
    }
  }

  @Test
  void shouldHandleCustomDenial() {
    RouteAccessDecision decision = RouteAccessDecision.deny("Custom reason");
    Location customLocation = new Location("/error");
    when(configuration.getCustomDenialLocation()).thenReturn(Optional.of(customLocation));

    try (MockedStatic<Router> routerMock = mockStatic(Router.class)) {
      Router router = mock(Router.class);
      routerMock.when(Router::getCurrent).thenReturn(router);

      securityManager.onAccessDenied(decision, navigationContext);

      verify(router).navigate(customLocation);
    }
  }

  @Route
  private static class TestRoute {
  }

  private static class TestSecurityManager extends AbstractRouteSecurityManager {
    private final RouteSecurityContext securityContext;
    private final RouteSecurityConfiguration configuration;

    TestSecurityManager(RouteSecurityContext securityContext,
        RouteSecurityConfiguration configuration) {
      this.securityContext = securityContext;
      this.configuration = configuration;
    }

    @Override
    public RouteSecurityContext getSecurityContext() {
      return securityContext;
    }

    @Override
    public RouteSecurityConfiguration getConfiguration() {
      return configuration;
    }
  }
}
