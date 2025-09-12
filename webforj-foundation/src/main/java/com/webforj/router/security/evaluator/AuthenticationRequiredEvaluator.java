package com.webforj.router.security.evaluator;

import com.webforj.router.NavigationContext;
import com.webforj.router.security.RouteAccessDecision;
import com.webforj.router.security.RouteSecurityContext;
import com.webforj.router.security.RouteSecurityEvaluator;
import com.webforj.router.security.SecurityEvaluatorChain;
import jakarta.annotation.security.PermitAll;
import jakarta.annotation.security.RolesAllowed;
import java.lang.annotation.Annotation;

/**
 * Evaluator that checks authentication requirements for security annotations.
 *
 * <p>
 * This evaluator ensures that routes with security annotations that require authentication
 * ({@code @PermitAll}, {@code @RolesAllowed}) are only accessible to authenticated users. It does
 * not grant or deny access itself, but ensures authentication is present before delegating to the
 * chain.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.10
 */
public class AuthenticationRequiredEvaluator implements RouteSecurityEvaluator {

  /**
   * {@inheritDoc}
   */
  @Override
  public RouteAccessDecision evaluate(Class<?> routeClass, NavigationContext context,
      RouteSecurityContext securityContext, SecurityEvaluatorChain chain) {

    // Check if route has security annotations that require authentication
    boolean requiresAuth = hasAnyAnnotation(routeClass, PermitAll.class, RolesAllowed.class);

    if (requiresAuth && !securityContext.isAuthenticated()) {
      // Stop chain - authentication required
      return RouteAccessDecision.denyAuthentication();
    }

    // Continue chain
    return chain.evaluate(routeClass, context, securityContext);
  }

  private boolean hasAnyAnnotation(Class<?> routeClass, Class<?>... annotationClasses) {
    for (Class<?> annotationClass : annotationClasses) {
      if (routeClass.isAnnotationPresent(annotationClass.asSubclass(Annotation.class))) {
        return true;
      }
    }
    return false;
  }
}
