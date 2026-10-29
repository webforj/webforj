package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.bbj.security.annotation.BBjPermission;
import com.webforj.bbj.security.model.BBjPermissions;
import com.webforj.router.NavigationContext;
import com.webforj.router.security.RouteAccessDecision;
import com.webforj.router.security.RouteSecurityContext;
import com.webforj.router.security.SecurityEvaluatorChain;
import java.util.Set;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class BBjPermissionEvaluatorTest {
  private final BBjPermissionEvaluator evaluator = new BBjPermissionEvaluator();
  private final NavigationContext navigationContext = mock(NavigationContext.class);
  private final RouteSecurityContext securityContext = mock(RouteSecurityContext.class);
  private final SecurityEvaluatorChain chain = mock(SecurityEvaluatorChain.class);

  @Test
  void shouldSupportRouteWithPermission() {
    assertTrue(evaluator.supports(AdminRoute.class));
  }

  @Test
  void shouldNotSupportRouteWithoutPermission() {
    assertFalse(evaluator.supports(PlainRoute.class));
  }

  @Test
  void shouldRequireAuthenticationOfUnsignedVisitor() {
    when(securityContext.isAuthenticated()).thenReturn(false);

    RouteAccessDecision decision =
        evaluator.evaluate(AdminRoute.class, navigationContext, securityContext, chain);

    assertTrue(decision.isAuthenticationRequired());
    verify(chain, never()).evaluate(any(), any(), any());
  }

  @ParameterizedTest
  @MethodSource("getGrantingCases")
  void shouldContinueTheChainWhenTheUserHasAnyListedPermission(Class<?> route,
      Set<String> authorities) {
    when(securityContext.isAuthenticated()).thenReturn(true);
    when(securityContext.hasAuthority(anyString()))
        .thenAnswer(invocation -> authorities.contains(invocation.getArgument(0)));
    RouteAccessDecision granted = RouteAccessDecision.grant();
    when(chain.evaluate(route, navigationContext, securityContext)).thenReturn(granted);

    RouteAccessDecision decision =
        evaluator.evaluate(route, navigationContext, securityContext, chain);

    assertSame(granted, decision);
  }

  @ParameterizedTest
  @MethodSource("getDenyingCases")
  void shouldDenyWhenTheUserHasNoListedPermission(Class<?> route, Set<String> authorities) {
    when(securityContext.isAuthenticated()).thenReturn(true);
    when(securityContext.hasAuthority(anyString()))
        .thenAnswer(invocation -> authorities.contains(invocation.getArgument(0)));

    RouteAccessDecision decision =
        evaluator.evaluate(route, navigationContext, securityContext, chain);

    assertTrue(decision.isAccessDenied());
    assertEquals("insufficient_permissions", decision.getReason());
    verify(chain, never()).evaluate(any(), any(), any());
  }

  private static Stream<Arguments> getGrantingCases() {
    return Stream.of(Arguments.of(AdminRoute.class, Set.of("admin-access")),
        Arguments.of(ConfigRoute.class, Set.of("server-config")),
        Arguments.of(ConfigRoute.class, Set.of("admin-access", "server-config")));
  }

  private static Stream<Arguments> getDenyingCases() {
    return Stream.of(Arguments.of(AdminRoute.class, Set.of()),
        Arguments.of(AdminRoute.class, Set.of("server-config")),
        Arguments.of(ConfigRoute.class, Set.of("user")),
        Arguments.of(EmptyRoute.class, Set.of("admin-access")));
  }

  @BBjPermission(BBjPermissions.ADMIN_ACCESS)
  private static final class AdminRoute {
  }

  @BBjPermission({BBjPermissions.ADMIN_ACCESS, BBjPermissions.SERVER_CONFIG})
  private static final class ConfigRoute {
  }

  @BBjPermission({})
  private static final class EmptyRoute {
  }

  private static final class PlainRoute {
  }
}
