package com.webforj.bbj.security.fixture;

import com.webforj.router.NavigationContext;
import com.webforj.router.security.RouteAccessDecision;
import com.webforj.router.security.RouteSecurityContext;
import com.webforj.bbj.security.BBjRouteSecurityEvaluator;
import com.webforj.router.security.SecurityEvaluatorChain;

/**
 * Evaluator fixture whose constructor fails.
 */
public class BrokenOwnerEvaluator implements BBjRouteSecurityEvaluator {

  /**
   * Fails on every creation.
   */
  public BrokenOwnerEvaluator() {
    throw new IllegalStateException("broken fixture");
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public RouteAccessDecision evaluate(Class<?> routeClass, NavigationContext context,
      RouteSecurityContext securityContext, SecurityEvaluatorChain chain) {
    return chain.evaluate(routeClass, context, securityContext);
  }
}
