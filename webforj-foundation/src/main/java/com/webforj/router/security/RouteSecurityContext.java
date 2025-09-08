package com.webforj.router.security;

import java.util.Optional;

/**
 * Security context for the current navigation request.
 *
 * <p>
 * Provides access to security-related information during route evaluation. Implementations should
 * provide authentication state, user identity, and authorization information.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.04
 */
public interface RouteSecurityContext {

  /**
   * Checks if the current user is authenticated.
   *
   * @return true if authenticated, false otherwise
   */
  boolean isAuthenticated();

  /**
   * Gets the principal (user identity) of the current user.
   *
   * @return the principal object, or empty if not authenticated
   */
  Optional<Object> getPrincipal();

  /**
   * Checks if the current user has a specific role.
   *
   * <p>
   * A role typically represents a broad set of permissions or a user category (e.g., "ADMIN",
   * "USER", "MANAGER"). Implementations may handle role naming conventions differently - some
   * frameworks add prefixes like "ROLE_" while others do not.
   * </p>
   *
   * @param role the role to check
   * @return true if the user has the role, false otherwise
   */
  boolean hasRole(String role);

  /**
   * Checks if the current user has a specific authority.
   *
   * <p>
   * An authority represents a specific permission or capability (e.g., "READ_USERS", "WRITE_POSTS",
   * "DELETE_COMMENTS"). Authorities are typically more granular than roles and represent individual
   * permissions rather than groups of permissions.
   * </p>
   *
   * @param authority the authority to check
   * @return true if the user has the authority, false otherwise
   */
  boolean hasAuthority(String authority);

  /**
   * Gets a security attribute by name.
   *
   * @param name the attribute name
   * @return the attribute value, or empty if not present
   */
  Optional<Object> getAttribute(String name);

  /**
   * Sets a security attribute.
   *
   * @param name the attribute name
   * @param value the attribute value
   */
  void setAttribute(String name, Object value);
}
