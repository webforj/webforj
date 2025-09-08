package com.webforj.router.security.evaluator;

import com.webforj.router.NavigationContext;
import com.webforj.router.security.RouteAccessDecision;
import com.webforj.router.security.RouteSecurityContext;
import com.webforj.router.security.RouteSecurityEvaluator;
import com.webforj.router.security.SecurityEvaluatorChain;
import jakarta.annotation.security.RolesAllowed;
import java.util.Arrays;

/**
 * Evaluator for {@link RolesAllowed} annotation.
 *
 * <p>
 * Routes marked with {@code @RolesAllowed} are accessible only to authenticated users who have at
 * least one of the specified roles. This evaluator checks both authentication and role membership.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.04
 */
public class RolesAllowedEvaluator implements RouteSecurityEvaluator {

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean supports(Class<?> routeClass) {
    return routeClass.isAnnotationPresent(RolesAllowed.class);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public RouteAccessDecision evaluate(Class<?> routeClass, NavigationContext context,
      RouteSecurityContext securityContext, SecurityEvaluatorChain chain) {

    // First check authentication
    if (!securityContext.isAuthenticated()) {
      return RouteAccessDecision.denyAuthentication();
    }

    // Get required roles from annotation
    RolesAllowed annotation = routeClass.getAnnotation(RolesAllowed.class);
    String[] requiredRoles = annotation.value();

    if (requiredRoles == null || requiredRoles.length == 0) {
      // No roles specified, deny
      return RouteAccessDecision.deny("No roles specified in @RolesAllowed");
    }

    // Check if user has any of the required roles
    boolean hasRequiredRole = Arrays.stream(requiredRoles).anyMatch(securityContext::hasRole);

    if (hasRequiredRole) {
      // User has required role - don't call chain
      return RouteAccessDecision.grant();
    }

    // User doesn't have required roles
    return RouteAccessDecision
        .denyPermissions("User lacks required roles: " + Arrays.toString(requiredRoles));
  }
}
