package com.webforj.bbj.security;

import com.webforj.bbj.security.model.BBjLoginStatus;
import com.webforj.bbj.security.model.BBjPermissions;
import com.webforj.bbj.security.model.BBjPrincipal;
import com.webforj.component.login.Login;
import com.webforj.component.login.event.LoginSubmitEvent;
import com.webforj.environment.ObjectTable;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

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
   * <p>
   * When the account needs a 2 factor code, the sign in ends as
   * {@link BBjLoginStatus#TWO_FACTOR_REQUIRED} and the service keeps a pending sign in for
   * {@code two-factor.pending-seconds}. {@link #submitCode(String)} completes it and
   * {@link #cancelLogin()} ends it. When {@code two-factor.code-path} is configured, the app also
   * navigates to that path.
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

    return openLocationOfStatus(getPendingStatus(userName, password));
  }

  /**
   * Signs a user in and asks for a 2 factor code when the account needs one.
   *
   * <p>
   * The sign in runs on the calling thread. It sends the password first. While BBj asks for a 2
   * factor code or refuses the given one, the code provider is asked for a code and the sign in is
   * repeated with it. The provider receives {@code false} on the first ask and {@code true} on each
   * ask after a refused code. When the provider returns {@code null} or a blank code, the sign in
   * stops and ends as {@link BBjLoginStatus#TWO_FACTOR_REQUIRED}. The sign in also stops after 5
   * refused codes and ends as {@link BBjLoginStatus#TWO_FACTOR_REQUIRED}. After a successful sign
   * in the app navigates to the location the user requested before signing in, or to the default
   * success path. When this method returns, the navigation has happened. No pending sign in is
   * kept.
   * </p>
   *
   * @param userName the user name
   * @param password the password
   * @param codeProvider gives the 2 factor code, or {@code null} or a blank code to stop the sign
   *        in
   *
   * @return the status of the sign in
   * @throws NullPointerException if any parameter is {@code null}
   */
  public static BBjLoginStatus login(String userName, String password,
      Function<Boolean, String> codeProvider) {
    Objects.requireNonNull(userName, "userName");
    Objects.requireNonNull(password, "password");
    Objects.requireNonNull(codeProvider, "codeProvider");

    return openTargetLocationOnSuccess(getStatus(userName, password, codeProvider));
  }

  /**
   * Signs in the user whose credentials a {@link Login} component submitted.
   *
   * <p>
   * The component shows no error while the sign in runs. When the credentials are invalid the
   * component shows its error state. When the sign in is unavailable, the component shows no error
   * and is enabled again. When the account needs a 2 factor code, the sign in ends as
   * {@link BBjLoginStatus#TWO_FACTOR_REQUIRED} with no error, and the service keeps a pending sign
   * in for {@code two-factor.pending-seconds}. {@link #submitCode(String)} completes it and
   * {@link #cancelLogin()} ends it. When {@code two-factor.code-path} is configured, the app
   * navigates to that path and the component is closed. After a successful sign in the component is
   * closed and then the app navigates to the location the user requested before signing in, or to
   * the default success path. Otherwise the component is enabled again.
   * </p>
   *
   * @param event the submit event of the component
   *
   * @return the status of the sign in
   * @throws NullPointerException if {@code event} is {@code null}
   * @throws ClassCastException if the source of the event is not a {@link Login}
   */
  public static BBjLoginStatus login(LoginSubmitEvent event) {
    return handleSubmit(event, () -> getPendingStatus(event.getUsername(), event.getPassword()),
        BBjSecurity::isCodeLocationOpened);
  }

  /**
   * Signs in the user whose credentials a {@link Login} component submitted and asks for a 2 factor
   * code when the account needs one.
   *
   * <p>
   * The component shows no error while the sign in runs. The sign in sends the password first.
   * While BBj asks for a 2 factor code or refuses the given one, the code provider is asked for a
   * code and the sign in is repeated with it. The provider receives {@code false} on the first ask
   * and {@code true} on each ask after a refused code. When the provider returns {@code null} or a
   * blank code, the sign in stops and ends as {@link BBjLoginStatus#TWO_FACTOR_REQUIRED} with no
   * error. The sign in also stops after 5 refused codes and ends as
   * {@link BBjLoginStatus#TWO_FACTOR_REQUIRED}. No pending sign in is kept.
   * </p>
   *
   * <p>
   * When the credentials are invalid the component shows its error state. When the sign in is
   * unavailable, the component shows no error. After a successful sign in the component is closed
   * and then the app navigates to the location the user requested before signing in, or to the
   * default success path. Otherwise the component is enabled again, also when the sign in or the
   * code provider throws.
   * </p>
   *
   * @param event the submit event of the component
   * @param codeProvider gives the 2 factor code, or {@code null} or a blank code to stop the sign
   *        in
   *
   * @return the status of the sign in
   * @throws NullPointerException if any parameter is {@code null}
   * @throws ClassCastException if the source of the event is not a {@link Login}
   */
  public static BBjLoginStatus login(LoginSubmitEvent event,
      Function<Boolean, String> codeProvider) {
    Objects.requireNonNull(event, "event");
    Objects.requireNonNull(codeProvider, "codeProvider");

    return handleSubmit(event,
        () -> getStatus(event.getUsername(), event.getPassword(), codeProvider), status -> false);
  }

  /**
   * Completes a sign in that waits for its 2 factor code.
   *
   * <p>
   * The sign in was started by {@link #login(String, String)} or {@link #login(LoginSubmitEvent)}
   * and ended as {@link BBjLoginStatus#TWO_FACTOR_REQUIRED}. The code is sent with the kept user
   * name and password. When BBj refuses the code, the status is
   * {@link BBjLoginStatus#TWO_FACTOR_REQUIRED} and the pending sign in stays for another code.
   * After a successful sign in the app navigates to the location the user requested before signing
   * in, or to the default success path. When this method returns, the navigation has happened.
   * </p>
   *
   * <p>
   * When no sign in is pending, the pending sign in is older than
   * {@code two-factor.pending-seconds} or the fifth code was refused, the status is
   * {@link BBjLoginStatus#CODE_EXPIRED}. A reload of the page ends a pending sign in too. The user
   * then signs in again with the password. Any status other than
   * {@link BBjLoginStatus#TWO_FACTOR_REQUIRED} ends the pending sign in. When BBj security is not
   * set up for the app, the status is {@link BBjLoginStatus#UNAVAILABLE}.
   * </p>
   *
   * @param code the 2 factor code
   *
   * @return the status of the code step
   * @throws NullPointerException if {@code code} is {@code null}
   */
  public static BBjLoginStatus submitCode(String code) {
    Objects.requireNonNull(code, "code");

    return openTargetLocationOnSuccess(
        getService().map(it -> it.sendCode(code)).orElse(BBjLoginStatus.UNAVAILABLE));
  }

  /**
   * Ends a sign in that waits for its 2 factor code.
   *
   * <p>
   * The kept user name and password are forgotten. When no sign in is pending, or BBj security is
   * not set up for the app, nothing happens.
   * </p>
   */
  public static void cancelLogin() {
    getService().ifPresent(BBjAuthenticationService::clearPending);
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
   *         signed in, BBj security is not set up, or the permission is {@code null}
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

  private static BBjLoginStatus handleSubmit(LoginSubmitEvent event,
      Supplier<BBjLoginStatus> signIn, Predicate<BBjLoginStatus> isRedirected) {
    Objects.requireNonNull(event, "event");

    Login login = (Login) event.getComponent();
    login.setError(false);
    BBjLoginStatus status;
    try {
      status = signIn.get();
    } catch (RuntimeException e) {
      login.setEnabled(true);
      throw e;
    }

    if (status == BBjLoginStatus.INVALID_CREDENTIALS) {
      login.setError(true);
    }

    if (status == BBjLoginStatus.AUTHENTICATED) {
      login.close();
      getService().ifPresent(BBjAuthenticationService::openTargetLocation);
    } else if (isRedirected.test(status)) {
      login.close();
      getService().ifPresent(BBjAuthenticationService::openCodeLocation);
    } else {
      login.setEnabled(true);
    }

    return status;
  }

  private static BBjLoginStatus openTargetLocationOnSuccess(BBjLoginStatus status) {
    if (status == BBjLoginStatus.AUTHENTICATED) {
      getService().ifPresent(BBjAuthenticationService::openTargetLocation);
    }

    return status;
  }

  private static BBjLoginStatus openLocationOfStatus(BBjLoginStatus status) {
    if (status == BBjLoginStatus.TWO_FACTOR_REQUIRED) {
      getService().ifPresent(BBjAuthenticationService::openCodeLocation);
    }

    return openTargetLocationOnSuccess(status);
  }

  private static boolean isCodeLocationOpened(BBjLoginStatus status) {
    return status == BBjLoginStatus.TWO_FACTOR_REQUIRED
        && getService().map(BBjAuthenticationService::hasCodeLocation).orElse(false);
  }

  private static BBjLoginStatus getPendingStatus(String userName, String password) {
    var status = sendLogin(userName, password, null);
    if (status == BBjLoginStatus.TWO_FACTOR_REQUIRED) {
      getService().ifPresent(it -> it.savePendingLogin(userName, password));
    }

    return status;
  }

  private static BBjLoginStatus getStatus(String userName, String password,
      Function<Boolean, String> codeProvider) {
    var status = sendLogin(userName, password, null);
    var retry = false;
    var attempts = 0;
    while (status == BBjLoginStatus.TWO_FACTOR_REQUIRED
        && attempts < BBjAuthenticationService.MAX_CODE_ATTEMPTS) {
      var code = codeProvider.apply(retry);
      if (code == null || code.isBlank()) {
        break;
      }

      retry = true;
      attempts++;
      status = sendLogin(userName, password, code);
    }

    return status;
  }

  private static BBjLoginStatus sendLogin(String userName, String password, String code) {
    return getService().map(it -> it.login(userName, password, code))
        .orElse(BBjLoginStatus.UNAVAILABLE);
  }

  private static Optional<BBjAuthenticationService> getService() {
    if (ObjectTable.contains(SERVICE_KEY)) {
      return Optional.of((BBjAuthenticationService) ObjectTable.get(SERVICE_KEY));
    }

    return Optional.empty();
  }
}
