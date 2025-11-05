package com.webforj.router.security.evaluator;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.router.NavigationContext;
import com.webforj.router.annotation.Route;
import com.webforj.router.security.RouteAccessDecision;
import com.webforj.router.security.RouteSecurityContext;
import com.webforj.router.security.SecurityEvaluatorChain;
import jakarta.annotation.security.RolesAllowed;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class RolesAllowedEvaluatorTest {

  private RolesAllowedEvaluator evaluator;
  private NavigationContext navigationContext;
  private RouteSecurityContext securityContext;
  private SecurityEvaluatorChain chain;

  @BeforeEach
  void setUp() {
    evaluator = new RolesAllowedEvaluator();
    navigationContext = mock(NavigationContext.class);
    securityContext = mock(RouteSecurityContext.class);
    chain = mock(SecurityEvaluatorChain.class);
  }

  @Test
  void shouldSupportRolesAllowedRoute() {
    assertTrue(evaluator.supports(SingleRoleRoute.class));
  }

  @Test
  void shouldNotSupportNormalRoute() {
    assertFalse(evaluator.supports(NormalRoute.class));
  }

  @Test
  void shouldDenyUnauthenticatedUser() {
    when(securityContext.isAuthenticated()).thenReturn(false);

    RouteAccessDecision decision =
        evaluator.evaluate(SingleRoleRoute.class, navigationContext, securityContext, chain);

    assertTrue(decision.isDenied());
    assertTrue(decision.isAuthenticationRequired());
  }

  @Test
  void shouldDelegateToChainWhenRoleCheckPasses() {
    when(securityContext.isAuthenticated()).thenReturn(true);
    when(securityContext.hasRole("ADMIN")).thenReturn(true);
    RouteAccessDecision chainDecision = RouteAccessDecision.grant();
    when(chain.evaluate(SingleRoleRoute.class, navigationContext, securityContext))
        .thenReturn(chainDecision);

    RouteAccessDecision decision =
        evaluator.evaluate(SingleRoleRoute.class, navigationContext, securityContext, chain);

    assertEquals(chainDecision, decision);
  }

  @Test
  void shouldDelegateToChainWithAnyMatchingRole() {
    when(securityContext.isAuthenticated()).thenReturn(true);
    when(securityContext.hasRole("ADMIN")).thenReturn(false);
    when(securityContext.hasRole("USER")).thenReturn(true);
    RouteAccessDecision chainDecision = RouteAccessDecision.grant();
    when(chain.evaluate(MultiRoleRoute.class, navigationContext, securityContext))
        .thenReturn(chainDecision);

    RouteAccessDecision decision =
        evaluator.evaluate(MultiRoleRoute.class, navigationContext, securityContext, chain);

    assertEquals(chainDecision, decision);
  }

  @Test
  void shouldDenyAccessWithoutRequiredRole() {
    when(securityContext.isAuthenticated()).thenReturn(true);
    when(securityContext.hasRole("ADMIN")).thenReturn(false);

    RouteAccessDecision decision =
        evaluator.evaluate(SingleRoleRoute.class, navigationContext, securityContext, chain);

    assertTrue(decision.isDenied());
    assertTrue(decision.isAccessDenied());
    assertEquals(RolesAllowedEvaluator.CODE, decision.getReason());
  }

  @Test
  void shouldDenyAccessForEmptyRoles() {
    when(securityContext.isAuthenticated()).thenReturn(true);

    RouteAccessDecision decision =
        evaluator.evaluate(EmptyRolesRoute.class, navigationContext, securityContext, chain);

    assertTrue(decision.isDenied());
    assertNotNull(decision.getReason());
  }

  @RolesAllowed("ADMIN")
  private static class SingleRoleRoute {
  }

  @RolesAllowed({"ADMIN", "USER"})
  private static class MultiRoleRoute {
  }

  @RolesAllowed({})
  private static class EmptyRolesRoute {
  }

  @Route
  private static class NormalRoute {
  }
}
