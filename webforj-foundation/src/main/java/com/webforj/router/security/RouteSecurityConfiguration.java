package com.webforj.router.security;

import com.webforj.router.history.Location;
import java.util.Optional;

/**
 * Configuration for route security behavior.
 *
 * <p>
 * Defines how the security system should handle authentication and authorization failures.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.04
 */
public interface RouteSecurityConfiguration {

  /**
   * Gets the location to redirect to when authentication is required.
   *
   * @return the authentication location, or empty to use default behavior
   */
  Optional<Location> getAuthenticationLocation();

  /**
   * Gets the location to redirect to when user lacks permissions.
   *
   * @return the insufficient permissions location, or empty to use default behavior
   */
  Optional<Location> getInsufficientPermissionsLocation();

  /**
   * Gets the location to redirect to for custom denials.
   *
   * <p>
   * Default implementation returns the insufficient permissions location.
   * </p>
   *
   * @return the custom denial location, or empty to use default behavior
   * @see #getInsufficientPermissionsLocation()
   */
  default Optional<Location> getCustomDenialLocation() {
    return getInsufficientPermissionsLocation();
  }

  /**
   * Checks if security is enabled.
   *
   * @return true if security is enabled, false otherwise
   */
  default boolean isEnabled() {
    return true;
  }

  /**
   * Checks if routes should be secured by default.
   *
   * <p>
   * When true, all routes require authentication unless explicitly marked as public. When false,
   * routes are public by default and must be explicitly secured.
   * </p>
   *
   * @return true for secure-by-default, false for public-by-default
   */
  default boolean isSecureByDefault() {
    return true;
  }
}
