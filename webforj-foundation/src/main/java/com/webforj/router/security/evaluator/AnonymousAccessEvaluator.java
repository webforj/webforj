package com.webforj.router.security.evaluator;

import com.webforj.router.NavigationContext;
import com.webforj.router.security.RouteAccessDecision;
import com.webforj.router.security.RouteSecurityContext;
import com.webforj.router.security.RouteSecurityEvaluator;
import com.webforj.router.security.SecurityEvaluatorChain;
import com.webforj.router.security.annotation.AnonymousAccess;

/**
 * Evaluator for {@link AnonymousAccess} annotation.
 *
 * <p>
 * Routes marked with {@code @AnonymousAccess} are accessible to all users, including anonymous
 * users. This evaluator should have high priority to bypass authentication requirements for public
 * routes.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.10
 */
public class AnonymousAccessEvaluator implements RouteSecurityEvaluator {

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean supports(Class<?> routeClass) {
    return routeClass.isAnnotationPresent(AnonymousAccess.class);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public RouteAccessDecision evaluate(Class<?> routeClass, NavigationContext context,
      RouteSecurityContext securityContext, SecurityEvaluatorChain chain) {
    // @AnonymousAccess allows everyone - don't call chain
    return RouteAccessDecision.grant();
  }
}
