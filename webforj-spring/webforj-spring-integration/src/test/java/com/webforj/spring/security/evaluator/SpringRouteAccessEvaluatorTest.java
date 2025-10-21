package com.webforj.spring.security.evaluator;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

import com.webforj.router.NavigationContext;
import com.webforj.router.security.RouteAccessDecision;
import com.webforj.router.security.RouteSecurityContext;
import com.webforj.router.security.SecurityEvaluatorChain;
import com.webforj.spring.security.annotation.RouteAccess;
import java.util.Arrays;
import java.util.Collection;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.User;

@ExtendWith(MockitoExtension.class)
class SpringRouteAccessEvaluatorTest {

  private SpringRouteAccessEvaluator evaluator;

  @Mock
  private NavigationContext navigationContext;

  @Mock
  private RouteSecurityContext securityContext;

  @Mock
  private SecurityEvaluatorChain chain;

  @Mock
  private SecurityContext springSecurityContext;

  private MockedStatic<SecurityContextHolder> securityContextHolderMock;

  @BeforeEach
  void setUp() {
    evaluator = new SpringRouteAccessEvaluator();
    securityContextHolderMock = Mockito.mockStatic(SecurityContextHolder.class);
  }

  @AfterEach
  void tearDown() {
    if (securityContextHolderMock != null) {
      securityContextHolderMock.close();
    }
  }

  @Nested
  class SupportsMethod {

    @Test
    void shouldSupportClassWithRouteAccessAnnotation() {
      assertTrue(evaluator.supports(RouteWithAccess.class));
    }

    @Test
    void shouldNotSupportClassWithoutRouteAccessAnnotation() {
      assertFalse(evaluator.supports(RouteWithoutAccess.class));
    }
  }

  @Nested
  class AuthenticationRequired {

    @Test
    void shouldDenyAccessWhenNotAuthenticated() {
      when(securityContext.isAuthenticated()).thenReturn(false);

      RouteAccessDecision decision =
          evaluator.evaluate(RouteWithAccess.class, navigationContext, securityContext, chain);

      assertFalse(decision.isGranted());
      assertEquals(RouteAccessDecision.AccessDenialType.AUTHENTICATION_REQUIRED,
          decision.getDenialType());
    }

    @Test
    void shouldRequireAuthenticationEvenForTrueExpression() {
      when(securityContext.isAuthenticated()).thenReturn(false);

      RouteAccessDecision decision =
          evaluator.evaluate(RouteWithAlwaysTrue.class, navigationContext, securityContext, chain);

      assertFalse(decision.isGranted());
      assertEquals(RouteAccessDecision.AccessDenialType.AUTHENTICATION_REQUIRED,
          decision.getDenialType());
    }
  }

  @Nested
  class ExpressionEvaluation {

    @BeforeEach
    void setupAuthentication() {
      when(securityContext.isAuthenticated()).thenReturn(true);

      Collection<GrantedAuthority> authorities = Arrays.asList(
          new SimpleGrantedAuthority("ROLE_USER"), new SimpleGrantedAuthority("ROLE_ADMIN"));
      User principal = new User("testuser", "password", authorities);
      Authentication authentication =
          new UsernamePasswordAuthenticationToken(principal, "password", authorities);

      securityContextHolderMock.when(SecurityContextHolder::getContext)
          .thenReturn(springSecurityContext);
      when(springSecurityContext.getAuthentication()).thenReturn(authentication);
    }

    @Test
    void shouldDenyAccessWhenMissingRole() {
      RouteAccessDecision decision =
          evaluator.evaluate(RouteWithRoleManager.class, navigationContext, securityContext, chain);

      assertFalse(decision.isGranted());
      assertEquals("MANAGER_REQUIRED", decision.getReason());
    }

    @Test
    void shouldDelegateToChainWhenHasAnyRole() {
      RouteAccessDecision chainDecision = RouteAccessDecision.grant();
      when(chain.evaluate(RouteWithAnyRole.class, navigationContext, securityContext))
          .thenReturn(chainDecision);

      RouteAccessDecision decision =
          evaluator.evaluate(RouteWithAnyRole.class, navigationContext, securityContext, chain);

      assertEquals(chainDecision, decision);
    }

    @Test
    void shouldDelegateToChainForAuthenticatedExpression() {
      RouteAccessDecision chainDecision = RouteAccessDecision.grant();
      when(chain.evaluate(RouteWithAuthenticated.class, navigationContext, securityContext))
          .thenReturn(chainDecision);

      RouteAccessDecision decision = evaluator.evaluate(RouteWithAuthenticated.class,
          navigationContext, securityContext, chain);

      assertEquals(chainDecision, decision);
    }

    @Test
    void shouldDelegateToChainWithCorrectRole() {
      Collection<GrantedAuthority> authorities = Arrays.asList(
          new SimpleGrantedAuthority("ROLE_USER"), new SimpleGrantedAuthority("ROLE_MANAGER"));
      User principal = new User("testuser", "password", authorities);
      Authentication authentication =
          new UsernamePasswordAuthenticationToken(principal, "password", authorities);

      securityContextHolderMock.when(SecurityContextHolder::getContext)
          .thenReturn(springSecurityContext);
      when(springSecurityContext.getAuthentication()).thenReturn(authentication);
      RouteAccessDecision chainDecision = RouteAccessDecision.grant();
      when(chain.evaluate(RouteWithRoleManager.class, navigationContext, securityContext))
          .thenReturn(chainDecision);

      RouteAccessDecision decision =
          evaluator.evaluate(RouteWithRoleManager.class, navigationContext, securityContext, chain);

      assertEquals(chainDecision, decision);
    }

    @Test
    void shouldDelegateToChainForComplexExpression() {
      RouteAccessDecision chainDecision = RouteAccessDecision.grant();
      when(chain.evaluate(RouteWithComplexExpression.class, navigationContext, securityContext))
          .thenReturn(chainDecision);

      RouteAccessDecision decision = evaluator.evaluate(RouteWithComplexExpression.class,
          navigationContext, securityContext, chain);

      assertEquals(chainDecision, decision);
    }

    @Test
    void shouldDenyComplexExpressionWhenUsernameDoesNotMatch() {
      Collection<GrantedAuthority> authorities = Arrays.asList(
          new SimpleGrantedAuthority("ROLE_USER"), new SimpleGrantedAuthority("ROLE_ADMIN"));
      User principal = new User("wronguser", "password", authorities);
      Authentication authentication =
          new UsernamePasswordAuthenticationToken(principal, "password", authorities);

      securityContextHolderMock.when(SecurityContextHolder::getContext)
          .thenReturn(springSecurityContext);
      when(springSecurityContext.getAuthentication()).thenReturn(authentication);

      RouteAccessDecision decision = evaluator.evaluate(RouteWithComplexExpression.class,
          navigationContext, securityContext, chain);

      assertFalse(decision.isGranted());
    }

  }

  @Nested
  class UnauthenticatedTests {

    @Test
    void shouldDenyWhenNotAuthenticatedForIsAuthenticatedExpression() {
      when(securityContext.isAuthenticated()).thenReturn(false);

      RouteAccessDecision decision = evaluator.evaluate(RouteWithAuthenticated.class,
          navigationContext, securityContext, chain);

      assertFalse(decision.isGranted());
      assertEquals(RouteAccessDecision.AccessDenialType.AUTHENTICATION_REQUIRED,
          decision.getDenialType());
    }
  }

  @RouteAccess(value = "hasRole('MANAGER')", code = "MANAGER_REQUIRED")
  static class RouteWithRoleManager {
  }

  @RouteAccess("hasAnyRole('USER', 'ADMIN')")
  static class RouteWithAnyRole {
  }

  @RouteAccess("isAuthenticated()")
  static class RouteWithAuthenticated {
  }

  @RouteAccess(value = "hasRole('MANAGER')", code = "MANAGER_REQUIRED")
  static class RouteWithCustomCode {
  }

  @RouteAccess("hasRole('ADMIN') && isAuthenticated() && #principal.username == 'testuser'")
  static class RouteWithComplexExpression {
  }

  @RouteAccess("true")
  static class RouteWithAlwaysTrue {
  }

  @RouteAccess(value = "this is not valid SpEL", code = "INVALID_EXPRESSION")
  static class RouteWithInvalidExpression {
  }

  @RouteAccess(value = "nonExistentMethod()", code = "ACCESS_DENIED")
  static class RouteWithErrorExpression {
  }

  @RouteAccess("true")
  static class RouteWithAccess {
  }

  @RouteAccess("#routeClass.simpleName == 'RouteWithVariableAccess'")
  static class RouteWithVariableAccess {
  }

  @RouteAccess("#authentication != null && #authentication.authenticated")
  static class RouteWithAuthenticationVariable {
  }

  static class RouteWithoutAccess {
  }
}
