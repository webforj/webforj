package com.webforj.router.security.evaluator;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verifyNoInteractions;

import com.webforj.router.NavigationContext;
import com.webforj.router.annotation.Route;
import com.webforj.router.security.RouteAccessDecision;
import com.webforj.router.security.RouteSecurityContext;
import com.webforj.router.security.SecurityEvaluatorChain;
import com.webforj.router.security.annotation.AnonymousAccess;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class AnonymousAccessEvaluatorTest {

  private AnonymousAccessEvaluator evaluator;
  private NavigationContext navigationContext;
  private RouteSecurityContext securityContext;
  private SecurityEvaluatorChain chain;

  @BeforeEach
  void setUp() {
    evaluator = new AnonymousAccessEvaluator();
    navigationContext = mock(NavigationContext.class);
    securityContext = mock(RouteSecurityContext.class);
    chain = mock(SecurityEvaluatorChain.class);
  }

  @Test
  void shouldSupportAnonymousRoute() {
    assertTrue(evaluator.supports(AnonymousRoute.class));
  }

  @Test
  void shouldNotSupportNormalRoute() {
    assertFalse(evaluator.supports(NormalRoute.class));
  }

  @Test
  void shouldGrantAccessForAnonymousRoute() {
    RouteAccessDecision decision =
        evaluator.evaluate(AnonymousRoute.class, navigationContext, securityContext, chain);

    assertTrue(decision.isGranted());
    verifyNoInteractions(chain);
  }

  @AnonymousAccess
  private static class AnonymousRoute {
  }

  @Route
  private static class NormalRoute {
  }
}
