package com.webforj.bbj.security.fixture;

import com.webforj.router.NavigationContext;
import com.webforj.router.security.RouteAccessDecision;
import com.webforj.router.security.RouteSecurityContext;
import com.webforj.router.security.RouteSecurityEvaluator;
import com.webforj.router.security.SecurityEvaluatorChain;

/**
 * Evaluator fixture that is not a BBjRouteSecurityEvaluator and denies every route with the reason
 * {@code plain}.
 */
public class PlainOwnerEvaluator implements RouteSecurityEvaluator {

  /**
   * {@inheritDoc}
   */
  @Override
  public RouteAccessDecision evaluate(Class<?> routeClass, NavigationContext context,
      RouteSecurityContext securityContext, SecurityEvaluatorChain chain) {
    return RouteAccessDecision.deny("plain");
  }
}
