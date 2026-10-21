package com.webforj.bbj.security.fixture;

import com.webforj.router.NavigationContext;
import com.webforj.router.security.RouteAccessDecision;
import com.webforj.router.security.RouteSecurityContext;
import com.webforj.bbj.security.BBjRouteSecurityEvaluator;
import com.webforj.router.security.SecurityEvaluatorChain;

/**
 * Evaluator fixture with the default priority that denies every route with the reason {@code mike}.
 */
public class MikeOwnerEvaluator implements BBjRouteSecurityEvaluator {

  /**
   * {@inheritDoc}
   */
  @Override
  public RouteAccessDecision evaluate(Class<?> routeClass, NavigationContext context,
      RouteSecurityContext securityContext, SecurityEvaluatorChain chain) {
    return RouteAccessDecision.deny("mike");
  }
}
