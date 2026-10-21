package com.webforj.bbj.security.fixture;

import com.webforj.bbj.security.BBjRouteSecurityEvaluator;
import com.webforj.router.NavigationContext;
import com.webforj.router.security.RouteAccessDecision;
import com.webforj.router.security.RouteSecurityContext;
import com.webforj.router.security.SecurityEvaluatorChain;

/**
 * Evaluator fixture with priority 0 that denies every route with the reason {@code first}.
 */
public class FirstOwnerEvaluator implements BBjRouteSecurityEvaluator {

  /**
   * {@inheritDoc}
   */
  @Override
  public int getPriority() {
    return 0;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public RouteAccessDecision evaluate(Class<?> routeClass, NavigationContext context,
      RouteSecurityContext securityContext, SecurityEvaluatorChain chain) {
    return RouteAccessDecision.deny("first");
  }
}
