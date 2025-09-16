package com.webforj.router.security;

import com.webforj.router.NavigationContext;
import com.webforj.router.history.Location;
import java.util.List;
import java.util.Optional;

/**
 * Central manager for route security.
 *
 * <p>
 * Coordinates security evaluation, manages evaluators, and handles access denial. This is the main
 * entry point for route security implementations.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.10
 */
public interface RouteSecurityManager {

  /**
   * Evaluates whether access should be granted to a route.
   *
   * <p>
   * Coordinates the evaluation process by obtaining the current security context and running all
   * registered evaluators in priority order.
   * </p>
   *
   * @param routeClass the route component class
   * @param context the navigation context
   *
   * @return access decision
   */
  RouteAccessDecision evaluate(Class<?> routeClass, NavigationContext context);

  /**
   * Called when access is denied.
   *
   * <p>
   * Handles the denial by performing appropriate actions such as redirecting to login or error
   * pages based on the denial type.
   * </p>
   *
   * @param decision the access decision that denied access
   * @param context the navigation context
   */
  void onAccessDenied(RouteAccessDecision decision, NavigationContext context);

  /**
   * Gets the current security context.
   *
   * @return the current security context.
   */
  RouteSecurityContext getSecurityContext();

  /**
   * Gets the security configuration.
   *
   * @return the security configuration
   */
  RouteSecurityConfiguration getConfiguration();

  /**
   * Registers a security evaluator with specified priority.
   *
   * <p>
   * Priority must be greater than 0. Lower values execute first.
   * </p>
   *
   * <p>
   * Typical priority ranges:
   * </p>
   * <ul>
   * <li>0.1 - 0.2: Security bypass evaluators (DenyAll, AnonymousAccess)</li>
   * <li>0.3 - 0.5: Authentication and authorization evaluators</li>
   * <li>0.6 - 0.9: Framework-specific evaluators (SpEL expressions, etc.)</li>
   * <li>1.0+: Custom user evaluators</li>
   * </ul>
   *
   * @param evaluator the evaluator to register
   * @param priority the priority (must be greater than 0)
   *
   * @throws IllegalArgumentException if priority is not greater than 0
   */
  void registerEvaluator(RouteSecurityEvaluator evaluator, double priority);

  /**
   * Unregisters a security evaluator.
   *
   * @param evaluator the evaluator to unregister
   */
  void unregisterEvaluator(RouteSecurityEvaluator evaluator);

  /**
   * Gets all registered evaluators.
   *
   * @return list of evaluators sorted by priority
   */
  List<RouteSecurityEvaluator> getEvaluators();

  /**
   * Gets the location that was requested before authentication redirect.
   *
   * <p>
   * Retrieves the location that the user was trying to access before being redirected to the
   * authentication page. This is typically used after successful authentication to redirect the
   * user back to their originally requested page.
   * </p>
   *
   * @return the pre-authentication location, or empty if none was stored
   * @since 25.10
   */
  Optional<Location> getPreAuthenticationLocation();

  /**
   * Clears the pre-authentication location.
   *
   * <p>
   * Removes the stored pre-authentication location from session storage. Should be called after
   * successfully redirecting the user to prevent stale redirects on subsequent logins.
   * </p>
   *
   * @since 25.10
   */
  void clearPreAuthenticationLocation();

  /**
   * Consumes the pre-authentication location.
   *
   * <p>
   * Gets and clears the pre-authentication location in a single operation. This is the recommended
   * method for handling post-login redirects.
   * </p>
   *
   * @return the pre-authentication location, or empty if none was stored
   * @since 25.10
   */
  default Optional<Location> consumePreAuthenticationLocation() {
    Optional<Location> location = getPreAuthenticationLocation();
    if (location.isPresent()) {
      clearPreAuthenticationLocation();
    }

    return location;
  }
}
