package com.webforj.bbj.security;

import com.webforj.router.security.RouteSecurityEvaluator;

/**
 * A route security evaluator that decides its own position in the evaluator chain.
 *
 * <p>
 * The built-in evaluators use the priorities 1 to 5. A priority is an integer and evaluators with a
 * lower priority run first. An evaluator that keeps the default priority runs after the built-in
 * ones, so a route that a built-in evaluator decides never reaches it. An evaluator with a priority
 * below 1 runs before the built-in ones and can decide any route first.
 * </p>
 *
 * <p>
 * Evaluators are listed in
 * {@code META-INF/services/com.webforj.bbj.security.BBjRouteSecurityEvaluator}. Evaluators of equal
 * priority run in the order in which they are listed there.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
public interface BBjRouteSecurityEvaluator extends RouteSecurityEvaluator {

  /**
   * Gets the priority of the evaluator.
   *
   * <p>
   * Evaluators with a lower priority run first. The default is 10.
   * </p>
   *
   * @return the priority
   */
  default int getPriority() {
    return 10;
  }

}
