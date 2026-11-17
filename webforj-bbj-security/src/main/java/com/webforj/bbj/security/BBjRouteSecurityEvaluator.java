package com.webforj.bbj.security;

import com.webforj.router.security.RouteSecurityEvaluator;

/**
 * A {@link RouteSecurityEvaluator} that an application adds to the route security of a BBj
 * application.
 *
 * <p>
 * An application implements this interface and lists the implementing class in the provider file
 * {@code META-INF/services/com.webforj.bbj.security.BBjRouteSecurityEvaluator}. The
 * {@link BBjRouteSecurityManager} registers each listed evaluator at its priority once the core
 * evaluators are registered. Priorities 0 to 9 belong to the core evaluators.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
public interface BBjRouteSecurityEvaluator extends RouteSecurityEvaluator {

  /**
   * Returns the priority at which the evaluator is registered. Lower values run first.
   *
   * @return the priority, 10 unless the evaluator overrides it
   */
  default int getPriority() {
    return 10;
  }
}
