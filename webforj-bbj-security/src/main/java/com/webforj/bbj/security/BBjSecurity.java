package com.webforj.bbj.security;

import com.webforj.Environment;
import com.webforj.PendingResult;
import com.webforj.bbj.security.model.BBjLoginStatus;
import com.webforj.bbj.security.model.BBjPrincipal;
import com.webforj.environment.ObjectTable;
import java.util.Objects;
import java.util.Optional;

/**
 * Entry point to the security state of the current app running in BBjServices.
 *
 * <p>
 * Every method answers for the app of the calling thread and agrees with the route security checks
 * of that app. An expired sign-in counts as anonymous. When route security is not enabled, or no
 * app is running, every method behaves as for an anonymous visitor.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
public final class BBjSecurity {

  private BBjSecurity() {}

  /**
   * Gets the signed-in user.
   *
   * @return the user, or empty when the visitor is anonymous
   */
  public static Optional<BBjPrincipal> currentUser() {
    return getContext().flatMap(BBjRouteSecurityContext::getActiveAuthentication)
        .map(authentication -> new BBjPrincipal(authentication.getUserName(),
            authentication.getRoles()));
  }

  /**
   * Checks whether the signed-in user has the given role.
   *
   * @param role the BBj security group name
   * @return {@code true} if the user is signed in and in that group
   *
   * @throws NullPointerException if the role is {@code null}
   */
  public static boolean hasRole(String role) {
    return role != null && getContext().map(context -> context.hasRole(role)).orElse(false);
  }

  /**
   * Checks whether the signed-in user has the given permission.
   *
   * <p>
   * Permissions are not available yet, so the result is always {@code false}.
   * </p>
   *
   * @param permission the permission name
   * @return {@code false}
   */
  public static boolean hasPermission(String permission) {
    return false;
  }

  /**
   * Requires a signed-in user.
   *
   * @return the signed-in user
   *
   * @throws SecurityException if the visitor is anonymous
   */
  public static BBjPrincipal require() {
    return currentUser().orElseThrow(
        () -> new SecurityException("Authentication is required, sign in and try again"));
  }

  /**
   * Requires a signed-in user with the given role.
   *
   * @param role the BBj security group name
   * @return the signed-in user
   *
   * @throws NullPointerException if the role is {@code null}
   * @throws SecurityException if the visitor is anonymous or does not have the role
   */
  public static BBjPrincipal require(String role) {
    Objects.requireNonNull(role, "role");
    BBjPrincipal principal = require();
    if (!principal.getRoles().contains(role)) {
      throw new SecurityException("Access denied, the role " + role + " is required");
    }

    return principal;
  }

  /**
   * Signs a user in.
   *
   * <p>
   * The call must run on the Environment thread of the app, for example in an event handler. The
   * Admin API call runs on a worker thread, within the time limit of the configuration, and the
   * returned result completes on the Environment thread. It never completes exceptionally.
   * </p>
   *
   * <p>
   * On {@link BBjLoginStatus#AUTHENTICATED} the user is signed in and the app has navigated to the
   * location the visitor asked for, or to the root. A blank user name or password answers
   * {@link BBjLoginStatus#INVALID_CREDENTIALS} without a server call. Cancelling the result before
   * it completes keeps the visitor signed out.
   * </p>
   *
   * @param userName the BBj user name
   * @param password the password of the user
   * @return the result of the sign-in
   *
   * @throws NullPointerException if an argument is {@code null}
   * @throws IllegalStateException if route security is not enabled or the call is not on the
   *         Environment thread of the app
   */
  public static PendingResult<BBjLoginStatus> login(String userName, String password) {
    if (Environment.getCurrent() == null) {
      throw new IllegalStateException(
          "Sign-in must be called on the Environment thread of the app");
    }

    String key = BBjAuthenticationService.class.getName();
    if (!ObjectTable.contains(key)) {
      throw new IllegalStateException("Sign-in is not available, route security is not enabled");
    }

    return ((BBjAuthenticationService) ObjectTable.get(key)).startLogin(userName, password);
  }

  private static Optional<BBjRouteSecurityContext> getContext() {
    String key = BBjRouteSecurityContext.class.getName();
    if (!ObjectTable.contains(key)) {
      return Optional.empty();
    }

    return Optional.of((BBjRouteSecurityContext) ObjectTable.get(key));
  }
}
