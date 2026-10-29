package com.webforj.bbj.security;

import com.webforj.router.NavigationContext;
import com.webforj.router.security.RouteAccessDecision;
import com.webforj.router.security.RouteSecurityContext;
import com.webforj.router.security.RouteSecurityEvaluator;
import com.webforj.router.security.SecurityEvaluatorChain;
import com.webforj.bbj.security.annotation.BBjPermission;
import java.util.Arrays;

/**
 * BBjPermissionEvaluator
 */
public final class BBjPermissionEvaluator implements RouteSecurityEvaluator {
  private static final String DENIAL_CODE = "insufficient_permissions";

  @Override
  public RouteAccessDecision evaluate(Class<?> routeClass, NavigationContext context,
      RouteSecurityContext securityContext, SecurityEvaluatorChain chain) {
    if (!securityContext.isAuthenticated()) {
      return RouteAccessDecision.denyAuthentication();
    }

    var permissions = routeClass.getAnnotation(BBjPermission.class).value();
    var hasPermissions = permissions.length != 0
        && Arrays.stream(permissions).map(Enum::toString).anyMatch(securityContext::hasAuthority);
    return hasPermissions ? chain.evaluate(routeClass, context, securityContext)
        : RouteAccessDecision.deny(DENIAL_CODE);
  }

  @Override
  public boolean supports(Class<?> routeClass) {
    return routeClass.isAnnotationPresent(BBjPermission.class);
  }


}
