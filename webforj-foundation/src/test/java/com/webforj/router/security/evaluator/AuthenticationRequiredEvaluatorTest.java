package com.webforj.router.security.evaluator;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.router.NavigationContext;
import com.webforj.router.annotation.Route;
import com.webforj.router.security.RouteAccessDecision;
import com.webforj.router.security.RouteSecurityContext;
import com.webforj.router.security.SecurityEvaluatorChain;
import jakarta.annotation.security.PermitAll;
import jakarta.annotation.security.RolesAllowed;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class AuthenticationRequiredEvaluatorTest {

  private AuthenticationRequiredEvaluator evaluator;
  private NavigationContext navigationContext;
  private RouteSecurityContext securityContext;
  private SecurityEvaluatorChain chain;

  @BeforeEach
  void setUp() {
    evaluator = new AuthenticationRequiredEvaluator();
    navigationContext = mock(NavigationContext.class);
    securityContext = mock(RouteSecurityContext.class);
    chain = mock(SecurityEvaluatorChain.class);
  }

  @Test
  void shouldRequireAuthenticationForPermitAll() {
    when(securityContext.isAuthenticated()).thenReturn(false);

    RouteAccessDecision decision =
        evaluator.evaluate(PermitAllRoute.class, navigationContext, securityContext, chain);

    assertTrue(decision.isDenied());
    assertTrue(decision.isAuthenticationRequired());
    verify(chain, never()).evaluate(any(), any(), any());
  }

  @Test
  void shouldRequireAuthenticationForRolesAllowed() {
    when(securityContext.isAuthenticated()).thenReturn(false);

    RouteAccessDecision decision =
        evaluator.evaluate(RolesAllowedRoute.class, navigationContext, securityContext, chain);

    assertTrue(decision.isDenied());
    assertTrue(decision.isAuthenticationRequired());
    verify(chain, never()).evaluate(any(), any(), any());
  }

  @Test
  void shouldProceedToChainForAuthenticatedUser() {
    when(securityContext.isAuthenticated()).thenReturn(true);
    when(chain.evaluate(any(), any(), any())).thenReturn(RouteAccessDecision.grant());

    RouteAccessDecision decision =
        evaluator.evaluate(PermitAllRoute.class, navigationContext, securityContext, chain);

    assertTrue(decision.isGranted());
    verify(chain).evaluate(PermitAllRoute.class, navigationContext, securityContext);
  }

  @Test
  void shouldAlwaysProceedToChainForNormalRoute() {
    when(securityContext.isAuthenticated()).thenReturn(false);
    when(chain.evaluate(any(), any(), any())).thenReturn(RouteAccessDecision.grant());

    RouteAccessDecision decision =
        evaluator.evaluate(NormalRoute.class, navigationContext, securityContext, chain);

    assertFalse(decision.isDenied());
    verify(chain).evaluate(NormalRoute.class, navigationContext, securityContext);
  }

  @PermitAll
  private static class PermitAllRoute {
  }

  @RolesAllowed({"USER"})
  private static class RolesAllowedRoute {
  }

  @Route
  private static class NormalRoute {
  }
}
