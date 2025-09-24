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
 * @since 25.10
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
     * Access is denied.
     *
     * <p>
     * User may be authenticated but access is denied for various reasons such as insufficient
     * permissions, IP restrictions, geographic limitations, time-based access, etc. The specific
     * reason is provided in the decision's reason field.
     * </p>
     */
    ACCESS_DENIED
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
   * Creates a decision that denies access.
   *
   * <p>
   * Use this for any access denial that isn't authentication-related, such as:
   * <ul>
   * <li>Insufficient permissions or roles</li>
   * <li>IP address restrictions</li>
   * <li>Geographic limitations</li>
   * <li>Time-based access restrictions</li>
   * <li>Rate limiting</li>
   * <li>License or subscription issues</li>
   * </ul>
   * The reason parameter should clearly explain why access was denied.
   * </p>
   *
   * @param reason specific reason for denial (e.g., "Insufficient permissions", "Access denied from
   *        your location")
   * @return access denied decision
   */
  public static RouteAccessDecision deny(String reason) {
    return new RouteAccessDecision(false, reason, AccessDenialType.ACCESS_DENIED);
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
   * Checks if denial is due to access denied (not authentication).
   *
   * @return true if denial type is ACCESS_DENIED
   */
  public boolean isAccessDenied() {
    return denialType == AccessDenialType.ACCESS_DENIED;
  }
}
