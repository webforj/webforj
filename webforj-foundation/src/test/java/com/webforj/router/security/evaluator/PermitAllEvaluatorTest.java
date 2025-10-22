package com.webforj.router.security.evaluator;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import com.webforj.router.NavigationContext;
import com.webforj.router.security.RouteAccessDecision;
import com.webforj.router.security.RouteSecurityContext;
import com.webforj.router.security.SecurityEvaluatorChain;
import jakarta.annotation.security.PermitAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class PermitAllEvaluatorTest {

  private PermitAllEvaluator evaluator;
  private NavigationContext navigationContext;
  private RouteSecurityContext securityContext;
  private SecurityEvaluatorChain chain;

  @BeforeEach
  void setUp() {
    evaluator = new PermitAllEvaluator();
    navigationContext = mock(NavigationContext.class);
    securityContext = mock(RouteSecurityContext.class);
    chain = mock(SecurityEvaluatorChain.class);
  }

  @Test
  void shouldSupportPermitAllRoute() {
    assertTrue(evaluator.supports(PermitAllRoute.class));
  }

  @Test
  void shouldNotSupportNormalRoute() {
    assertFalse(evaluator.supports(NormalRoute.class));
  }

  @Test
  void shouldGrantAccessForAuthenticatedUser() {
    when(securityContext.isAuthenticated()).thenReturn(true);

    RouteAccessDecision decision =
        evaluator.evaluate(PermitAllRoute.class, navigationContext, securityContext, chain);

    assertTrue(decision.isGranted());
    verifyNoInteractions(chain);
  }

  @Test
  void shouldDenyAccessForUnauthenticatedUser() {
    when(securityContext.isAuthenticated()).thenReturn(false);

    RouteAccessDecision decision =
        evaluator.evaluate(PermitAllRoute.class, navigationContext, securityContext, chain);

    assertTrue(decision.isDenied());
    assertTrue(decision.isAuthenticationRequired());
    verifyNoInteractions(chain);
  }

  @PermitAll
  private static class PermitAllRoute {
  }

  private static class NormalRoute {
  }
}
