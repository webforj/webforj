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
 * @since 25.10
 */
public interface RouteSecurityConfiguration {

  /**
   * Gets the location to redirect to when authentication is required.
   *
   * @return the authentication location, or empty to use default behavior
   */
  default Optional<Location> getAuthenticationLocation() {
    return Optional.of(new Location("/login"));
  }

  /**
   * Gets the location to redirect to when access is denied.
   *
   * <p>
   * This is used for both insufficient permissions and custom denials.
   * </p>
   *
   * @return the deny location, or empty to use default behavior
   */
  default Optional<Location> getDenyLocation() {
    Optional<Location> authLocation = getAuthenticationLocation();
    if (authLocation.isPresent()) {
      authLocation.get().getQueryParameters().put("error", "");
      return authLocation;
    }

    return Optional.empty();
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
