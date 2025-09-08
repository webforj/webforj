package com.webforj.router.security;

/**
 * Result of route access evaluation.
 *
 * <p>
 * Encapsulates the decision about whether access should be granted to a route, along with
 * contextual information about why access was denied if applicable.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.04
 */
public final class RouteAccessDecision {
  private final boolean granted;
  private final String reason;
  private final AccessDenialType denialType;

  /**
   * Type of access denial.
   */
  public enum AccessDenialType {
    /**
     * User is not authenticated.
     *
     * <p>
     * Indicates the user needs to authenticate before accessing the route. Typically results in
     * redirect to login page.
     * </p>
     */
    AUTHENTICATION_REQUIRED,

    /**
     * User lacks required permissions.
     *
     * <p>
     * User is authenticated but doesn't have the necessary roles or authorities. Typically results
     * in a forbidden error page.
     * </p>
     */
    INSUFFICIENT_PERMISSIONS,

    /**
     * Custom denial reason.
     *
     * <p>
     * Used for application-specific access denial scenarios that don't fit the standard categories.
     *
     * The application should handle this denial type through custom logic, potentially showing
     * error messages, modals, or performing custom navigation.
     * </p>
     */
    CUSTOM_DENIAL
  }

  private RouteAccessDecision(boolean granted, String reason, AccessDenialType denialType) {
    if (!granted && reason == null) {
      throw new IllegalArgumentException("Denial reason cannot be null when access is denied");
    }

    if (!granted && denialType == null) {
      throw new IllegalArgumentException("Denial type cannot be null when access is denied");
    }

    if (granted && (reason != null || denialType != null)) {
      throw new IllegalArgumentException("Reason and type must be null when access is granted");
    }

    this.granted = granted;
    this.reason = reason;
    this.denialType = denialType;
  }

  /**
   * Creates a decision that grants access.
   *
   * @return access granted decision
   */
  public static RouteAccessDecision grant() {
    return new RouteAccessDecision(true, null, null);
  }

  /**
   * Creates a decision that denies access due to missing authentication.
   *
   * <p>
   * Use this when the user needs to authenticate before accessing the route.
   * </p>
   *
   * @return authentication required decision with standard message
   */
  public static RouteAccessDecision denyAuthentication() {
    return denyAuthentication("Authentication required");
  }

  /**
   * Creates a decision that denies access due to missing authentication with custom message.
   *
   * @param reason specific reason for authentication requirement
   * @return authentication required decision
   */
  public static RouteAccessDecision denyAuthentication(String reason) {
    return new RouteAccessDecision(false, reason, AccessDenialType.AUTHENTICATION_REQUIRED);
  }

  /**
   * Creates a decision that denies access due to insufficient permissions.
   *
   * @param reason description of missing permissions
   * @return insufficient permissions decision
   */
  public static RouteAccessDecision denyPermissions(String reason) {
    return new RouteAccessDecision(false, reason, AccessDenialType.INSUFFICIENT_PERMISSIONS);
  }

  /**
   * Creates a decision that denies access for a custom reason.
   *
   * @param reason custom denial reason
   * @return custom denial decision
   */
  public static RouteAccessDecision deny(String reason) {
    return new RouteAccessDecision(false, reason, AccessDenialType.CUSTOM_DENIAL);
  }

  /**
   * Checks if access is granted.
   *
   * @return true if access is granted, false otherwise
   */
  public boolean isGranted() {
    return granted;
  }

  /**
   * Checks if access is denied.
   *
   * @return true if access is denied, false otherwise
   */
  public boolean isDenied() {
    return !granted;
  }

  /**
   * Gets the reason for denial.
   *
   * <p>
   * Provides human-readable explanation of why access was denied. This can be used for logging or
   * displaying error messages.
   * </p>
   *
   * @return denial reason or null if access is granted
   */
  public String getReason() {
    return reason;
  }

  /**
   * Gets the type of denial.
   *
   * <p>
   * Used to determine appropriate handling such as redirecting to login page vs showing forbidden
   * error.
   * </p>
   *
   * @return denial type or null if access is granted
   */
  public AccessDenialType getDenialType() {
    return denialType;
  }

  /**
   * Checks if denial is due to missing authentication.
   *
   * @return true if denial type is AUTHENTICATION_REQUIRED
   */
  public boolean isAuthenticationRequired() {
    return denialType == AccessDenialType.AUTHENTICATION_REQUIRED;
  }

  /**
   * Checks if denial is due to insufficient permissions.
   *
   * @return true if denial type is INSUFFICIENT_PERMISSIONS
   */
  public boolean isInsufficientPermissions() {
    return denialType == AccessDenialType.INSUFFICIENT_PERMISSIONS;
  }
}
