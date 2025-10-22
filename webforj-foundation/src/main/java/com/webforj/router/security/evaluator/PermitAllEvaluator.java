package com.webforj.router.security.evaluator;

import com.webforj.router.NavigationContext;
import com.webforj.router.security.RouteAccessDecision;
import com.webforj.router.security.RouteSecurityContext;
import com.webforj.router.security.RouteSecurityEvaluator;
import com.webforj.router.security.SecurityEvaluatorChain;
import jakarta.annotation.security.PermitAll;

/**
 * Evaluator for {@link PermitAll} annotation.
 *
 * <p>
 * Routes marked with {@code @PermitAll} are accessible to all authenticated users, regardless of
 * their roles. This is different from {@code @AnonymousAccess} which allows unauthenticated access.
 * {@code @PermitAll} still requires authentication.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.10
 */
public class PermitAllEvaluator implements RouteSecurityEvaluator {

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean supports(Class<?> routeClass) {
    return routeClass.isAnnotationPresent(PermitAll.class);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public RouteAccessDecision evaluate(Class<?> routeClass, NavigationContext context,
      RouteSecurityContext securityContext, SecurityEvaluatorChain chain) {
    // @PermitAll requires authentication but allows all authenticated users
    if (!securityContext.isAuthenticated()) {
      return RouteAccessDecision.denyAuthentication();
    }

    // User is authenticated - grant access (don't call chain)
    // @PermitAll is a complete decision: any authenticated user is allowed
    return RouteAccessDecision.grant();
  }
}
