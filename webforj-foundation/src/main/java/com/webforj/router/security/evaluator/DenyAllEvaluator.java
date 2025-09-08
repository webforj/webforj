package com.webforj.router.security.evaluator;

import com.webforj.router.NavigationContext;
import com.webforj.router.security.RouteAccessDecision;
import com.webforj.router.security.RouteSecurityContext;
import com.webforj.router.security.RouteSecurityEvaluator;
import com.webforj.router.security.SecurityEvaluatorChain;
import jakarta.annotation.security.DenyAll;

/**
 * Evaluator for {@link DenyAll} annotation.
 *
 * <p>
 * Routes marked with {@code @DenyAll} are always denied access, regardless of authentication or
 * authorization status. This evaluator should have the highest priority to ensure it runs first.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.04
 */
public class DenyAllEvaluator implements RouteSecurityEvaluator {

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean supports(Class<?> routeClass) {
    return routeClass.isAnnotationPresent(DenyAll.class);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public RouteAccessDecision evaluate(Class<?> routeClass, NavigationContext context,
      RouteSecurityContext securityContext, SecurityEvaluatorChain chain) {
    // @DenyAll always denies - don't call chain
    return RouteAccessDecision.deny("Access denied by @DenyAll annotation");
  }
}
