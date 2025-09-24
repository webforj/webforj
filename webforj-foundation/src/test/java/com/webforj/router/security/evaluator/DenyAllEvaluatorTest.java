package com.webforj.router.security.evaluator;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import com.webforj.router.NavigationContext;
import com.webforj.router.annotation.Route;
import com.webforj.router.security.RouteAccessDecision;
import com.webforj.router.security.RouteAccessDecision.AccessDenialType;
import com.webforj.router.security.RouteSecurityContext;
import com.webforj.router.security.SecurityEvaluatorChain;
import jakarta.annotation.security.DenyAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class DenyAllEvaluatorTest {

  private DenyAllEvaluator evaluator;
  private NavigationContext navigationContext;
  private RouteSecurityContext securityContext;
  private SecurityEvaluatorChain chain;

  @BeforeEach
  void setUp() {
    evaluator = new DenyAllEvaluator();
    navigationContext = mock(NavigationContext.class);
    securityContext = mock(RouteSecurityContext.class);
    chain = mock(SecurityEvaluatorChain.class);
  }

  @Test
  void shouldSupportDenyAllRoute() {
    assertTrue(evaluator.supports(DenyAllRoute.class));
  }

  @Test
  void shouldNotSupportNormalRoute() {
    assertFalse(evaluator.supports(NormalRoute.class));
  }

  @Test
  void shouldAlwaysDenyAccess() {
    RouteAccessDecision decision =
        evaluator.evaluate(DenyAllRoute.class, navigationContext, securityContext, chain);

    assertTrue(decision.isDenied());
    assertEquals(DenyAllEvaluator.CODE, decision.getReason());
    assertEquals(AccessDenialType.ACCESS_DENIED, decision.getDenialType());
    verifyNoInteractions(chain);
  }

  @Test
  void shouldDenyEvenAuthenticatedUser() {
    when(securityContext.isAuthenticated()).thenReturn(true);
    when(securityContext.hasRole("ADMIN")).thenReturn(true);

    RouteAccessDecision decision =
        evaluator.evaluate(DenyAllRoute.class, navigationContext, securityContext, chain);

    assertTrue(decision.isDenied());
    assertEquals(DenyAllEvaluator.CODE, decision.getReason());
    verifyNoInteractions(chain);
  }

  @DenyAll
  private static class DenyAllRoute {
  }

  @Route
  private static class NormalRoute {
  }
}
