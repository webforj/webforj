package com.webforj.router.security;

import com.webforj.router.NavigationContext;

/**
 * Represents a chain of security evaluators.
 *
 * <p>
 * The chain allows evaluators to delegate evaluation to the next evaluator in the chain. This
 * enables each evaluator to decide whether to handle the evaluation, pass it to the next evaluator,
 * or stop the chain.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.10
 */
public interface SecurityEvaluatorChain {

  /**
   * Continues evaluation with the next evaluator in the chain.
   *
   * <p>
   * When an evaluator calls this method, control is passed to the next evaluator that supports the
   * given route class. If no more evaluators are available, the default security behavior is
   * applied based on the configuration.
   * </p>
   *
   * @param routeClass the route component class to evaluate
   * @param context the navigation context
   * @param securityContext the security context
   *
   * @return the access decision from the chain
   */
  RouteAccessDecision evaluate(Class<?> routeClass, NavigationContext context,
      RouteSecurityContext securityContext);
}
