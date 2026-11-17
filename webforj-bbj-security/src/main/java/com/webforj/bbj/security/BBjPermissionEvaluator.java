package com.webforj.bbj.security;

import com.webforj.bbj.security.annotation.BBjPermission;
import com.webforj.router.NavigationContext;
import com.webforj.router.security.RouteAccessDecision;
import com.webforj.router.security.RouteSecurityContext;
import com.webforj.router.security.RouteSecurityEvaluator;
import com.webforj.router.security.SecurityEvaluatorChain;
import java.util.Arrays;

/**
 * {@link RouteSecurityEvaluator} of routes annotated with {@link BBjPermission}.
 *
 * <p>
 * A user who has not signed in is asked to authenticate. A signed in user who holds any of the
 * listed permissions is passed on to the next evaluator. Every other user is denied access with the
 * reason {@code insufficient_permissions}.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
public final class BBjPermissionEvaluator implements RouteSecurityEvaluator {
  private static final String DENIAL_CODE = "insufficient_permissions";

  /**
   * {@inheritDoc}
   */
  @Override
  public RouteAccessDecision evaluate(Class<?> routeClass, NavigationContext context,
      RouteSecurityContext securityContext, SecurityEvaluatorChain chain) {
    if (!securityContext.isAuthenticated()) {
      return RouteAccessDecision.denyAuthentication();
    }

    var permissions = routeClass.getAnnotation(BBjPermission.class).value();
    var hasPermissions =
        Arrays.stream(permissions).map(Enum::toString).anyMatch(securityContext::hasAuthority);

    return hasPermissions ? chain.evaluate(routeClass, context, securityContext)
        : RouteAccessDecision.deny(DENIAL_CODE);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean supports(Class<?> routeClass) {
    return routeClass.isAnnotationPresent(BBjPermission.class);
  }
}
