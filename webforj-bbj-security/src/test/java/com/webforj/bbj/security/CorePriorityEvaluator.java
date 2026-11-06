package com.webforj.bbj.security;

import com.webforj.router.NavigationContext;
import com.webforj.router.security.RouteAccessDecision;
import com.webforj.router.security.RouteSecurityContext;
import com.webforj.router.security.SecurityEvaluatorChain;

/**
 * An evaluator listed in the provider file with a priority of the core evaluators.
 */
public class CorePriorityEvaluator implements BBjRouteSecurityEvaluator {

  /**
   * Passes the route on to the next evaluator.
   */
  @Override
  public RouteAccessDecision evaluate(Class<?> routeClass, NavigationContext context,
      RouteSecurityContext securityContext, SecurityEvaluatorChain chain) {
    return chain.evaluate(routeClass, context, securityContext);
  }

  /**
   * Returns 9, a priority in the range of the core evaluators.
   */
  @Override
  public int getPriority() {
    return 9;
  }
}
