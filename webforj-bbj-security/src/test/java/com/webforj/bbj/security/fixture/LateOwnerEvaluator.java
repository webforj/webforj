package com.webforj.bbj.security.fixture;

import com.webforj.bbj.security.BBjRouteSecurityEvaluator;
import com.webforj.router.NavigationContext;
import com.webforj.router.security.RouteAccessDecision;
import com.webforj.router.security.RouteSecurityContext;
import com.webforj.router.security.SecurityEvaluatorChain;

/**
 * Evaluator fixture with priority 20 that denies every route with the reason {@code late}.
 */
public class LateOwnerEvaluator implements BBjRouteSecurityEvaluator {

  /**
   * {@inheritDoc}
   */
  @Override
  public int getPriority() {
    return 20;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public RouteAccessDecision evaluate(Class<?> routeClass, NavigationContext context,
      RouteSecurityContext securityContext, SecurityEvaluatorChain chain) {
    return RouteAccessDecision.deny("late");
  }
}
