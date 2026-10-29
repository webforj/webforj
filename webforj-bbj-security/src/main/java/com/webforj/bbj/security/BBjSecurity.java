package com.webforj.bbj.security;

import com.webforj.bbj.security.model.BBjLoginStatus;
import com.webforj.bbj.security.model.BBjPermissions;
import com.webforj.bbj.security.model.BBjPrincipal;
import com.webforj.component.Theme;
import com.webforj.component.login.Login;
import com.webforj.component.login.event.LoginSubmitEvent;
import com.webforj.component.toast.Toast;
import com.webforj.environment.ObjectTable;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Predicate;

/**
 * Entry point of an app to sign users in and out against BBj and to read the user that signed in.
 *
 * <p>
 * The calls work on the {@link BBjAuthenticationService} of the current environment. When BBj
 * security is not set up for the app, {@code login} reports {@link BBjLoginStatus#UNAVAILABLE},
 * {@code logout} does nothing and {@code currentUser} is empty.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
public final class BBjSecurity {
  /**
   * The key under which the {@link BBjAuthenticationService} is stored in the object table.
   */
  public static final String SERVICE_KEY = BBjAuthenticationService.class.getName();
  private static final String UNAVAILABLE_TEXT = "Cannot sign in right now.";
  private static final String THROTTLED_TEXT = "Too many sign in attempts. Try again later.";

  private BBjSecurity() {}

  /**
   * Signs a user in.
   *
   * <p>
   * The sign in runs on the calling thread. After a successful sign in the app navigates to the
   * location the user requested before signing in, or to the default success path. When this method
   * returns, the navigation has happened.
   * </p>
   *
   * @param userName the user name
   * @param password the password
   *
   * @return the status of the sign in
   * @throws NullPointerException if any parameter is {@code null}
   */
  public static BBjLoginStatus login(String userName, String password) {
    Objects.requireNonNull(userName, "userName");
    Objects.requireNonNull(password, "password");

    var status = getStatus(userName, password);
    if (status == BBjLoginStatus.AUTHENTICATED) {
      getService().ifPresent(BBjAuthenticationService::openTargetLocation);
    }

    return status;
  }

  /**
   * Signs in the user whose credentials a {@link Login} component submitted.
   *
   * <p>
   * The component shows no error while the sign in runs. When the credentials are invalid the
   * component shows its error state. When the sign in is unavailable or refused after too many
   * failed attempts a warning toast is shown. After a successful sign in the component is closed
   * and then the app navigates to the location the user requested before signing in, or to the
   * default success path. Otherwise the component is enabled again.
   * </p>
   *
   * @param event the submit event of the component
   *
   * @return the status of the sign in
   * @throws NullPointerException if {@code event} is {@code null}
   * @throws ClassCastException if the source of the event is not a {@link Login}
   */
  public static BBjLoginStatus login(LoginSubmitEvent event) {
    Objects.requireNonNull(event, "event");

    Login login = (Login) event.getComponent();
    login.setError(false);
    BBjLoginStatus status;
    try {
      status = getStatus(event.getUsername(), event.getPassword());
    } catch (RuntimeException e) {
      login.setEnabled(true);
      throw e;
    }

    if (status == BBjLoginStatus.INVALID_CREDENTIALS) {
      login.setError(true);
    } else if (status == BBjLoginStatus.UNAVAILABLE) {
      Toast.show(UNAVAILABLE_TEXT, Theme.WARNING);
    } else if (status == BBjLoginStatus.THROTTLED) {
      Toast.show(THROTTLED_TEXT, Theme.WARNING);
    }

    if (status == BBjLoginStatus.AUTHENTICATED) {
      login.close();
      getService().ifPresent(BBjAuthenticationService::openTargetLocation);
    } else {
      login.setEnabled(true);
    }

    return status;
  }

  /**
   * Signs the current user out.
   *
   * <p>
   * When nobody has signed in, nothing happens. Otherwise the user is signed out at once, the
   * requested location is forgotten and the app opens the logout location. Then the login token is
   * ended. A failure to end it is logged and the token expires on its own. When BBj security is not
   * set up for the app, nothing happens.
   * </p>
   */
  public static void logout() {
    getService().ifPresent(BBjAuthenticationService::logout);
  }

  /**
   * Checks whether the user that signed in holds a role.
   *
   * <p>
   * The roles of a user are the names of the BBj security groups the user is a member of. The name
   * is compared exactly, case sensitive.
   * </p>
   *
   * @param role the name of the role
   *
   * @return {@code true} if the signed in user holds the role, {@code false} if nobody has signed
   *         in, BBj security is not set up, or the role is {@code null}
   */
  public static boolean hasRole(String role) {
    return role != null && currentUser().map(BBjPrincipal::getRoles)
        .filter(createContainsPredicate(role)).isPresent();
  }

  /**
   * Checks whether the user that signed in holds an authority.
   *
   * <p>
   * The authorities of a user are the names of the {@link BBjPermissions} the user holds. The name
   * is compared exactly, case sensitive.
   * </p>
   *
   * @param authority the name of the authority
   *
   * @return {@code true} if the signed in user holds the authority, {@code false} if nobody has
   *         signed in, BBj security is not set up, or the authority is {@code null}
   */
  public static boolean hasAuthority(String authority) {
    return authority != null && currentUser().map(BBjPrincipal::getAuthorities)
        .filter(createContainsPredicate(authority)).isPresent();
  }

  /**
   * Checks whether the user that signed in holds a {@link BBjPermissions} permission.
   *
   * @param permission the permission
   *
   * @return {@code true} if the signed in user holds the permission, {@code false} if nobody has
   *         signed in or BBj security is not set up
   * @throws NullPointerException if {@code permission} is {@code null}
   */
  public static boolean hasAuthority(BBjPermissions permission) {
    return permission != null && hasAuthority(permission.toString());
  }

  /**
   * Gets the user that signed in.
   *
   * @return the signed in user, or empty if nobody has signed in or BBj security is not set up
   */
  public static Optional<BBjPrincipal> currentUser() {
    return getService().flatMap(BBjAuthenticationService::currentUser);
  }

  private static Predicate<Set<String>> createContainsPredicate(String value) {
    return set -> set.contains(value);
  }

  private static BBjLoginStatus getStatus(String userName, String password) {
    return getService().map(it -> it.login(userName, password)).orElse(BBjLoginStatus.UNAVAILABLE);
  }

  private static Optional<BBjAuthenticationService> getService() {
    if (ObjectTable.contains(SERVICE_KEY)) {
      return Optional.of((BBjAuthenticationService) ObjectTable.get(SERVICE_KEY));
    }

    return Optional.empty();
  }
}
