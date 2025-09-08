package com.webforj.router.security;

import com.webforj.router.NavigationContext;

/**
 * Evaluates security requirements for routes in a chain pattern.
 *
 * <p>
 * Implementations participate in a chain of responsibility pattern where each evaluator can decide
 * to grant access, deny access, or delegate to the next evaluator in the chain.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.04
 */
public interface RouteSecurityEvaluator {

  /**
   * Evaluates whether access should be granted to a route.
   *
   * <p>
   * The evaluator can make one of three decisions:
   * </p>
   * <ul>
   * <li>Grant access by returning {@link RouteAccessDecision#grant()}</li>
   * <li>Deny access by returning a denial decision</li>
   * <li>Delegate to the next evaluator by calling {@code chain.evaluate()}</li>
   * </ul>
   *
   * @param routeClass the route component class
   * @param context the navigation context
   * @param securityContext the current security context
   * @param chain the evaluator chain for delegation
   *
   * @return access decision
   */
  RouteAccessDecision evaluate(Class<?> routeClass, NavigationContext context,
      RouteSecurityContext securityContext, SecurityEvaluatorChain chain);

  /**
   * Checks if this evaluator supports the given route class.
   *
   * <p>
   * Only evaluators that return {@code true} will be invoked for the given route. This allows
   * evaluators to filter which routes they process.
   * </p>
   *
   * @param routeClass the route component class
   * @return true if this evaluator can handle the route, false otherwise
   */
  default boolean supports(Class<?> routeClass) {
    return true;
  }
}
