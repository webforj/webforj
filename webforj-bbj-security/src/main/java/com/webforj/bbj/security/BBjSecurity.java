package com.webforj.bbj.security;

import com.webforj.PendingResult;
import com.webforj.bbj.security.model.BBjPrincipal;
import com.webforj.bbj.security.model.BBjLoginStatus;
import com.webforj.component.ComponentLifecycleObserver;
import com.webforj.component.ComponentLifecycleObserver.LifecycleEvent;
import com.webforj.component.Theme;
import com.webforj.component.login.Login;
import com.webforj.component.login.event.LoginSubmitEvent;
import com.webforj.component.toast.Toast;
import com.webforj.environment.ObjectTable;
import java.util.Objects;
import java.util.Optional;

/**
 * Entry point of an app to sign users in against BBj and to read the user that signed in.
 *
 * <p>
 * The calls work on the {@link BBjAuthenticationService} of the current environment. When BBj
 * security is not set up for the app, {@code login} reports {@link BBjLoginStatus#UNAVAILABLE} and
 * {@code currentUser} is empty.
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

  private BBjSecurity() {}

  /**
   * Signs a user in.
   *
   * <p>
   * The sign in runs in the background and the pending result completes with its status. After a
   * successful sign in the app navigates to the location the user requested before signing in, or
   * to the default success path.
   * </p>
   *
   * @param userName the user name
   * @param password the password
   *
   * @return the pending status of the sign in
   * @throws NullPointerException if any parameter is {@code null}
   */
  public static PendingResult<BBjLoginStatus> login(String userName, String password) {
    Objects.requireNonNull(userName, "userName");
    Objects.requireNonNull(password, "password");

    return getService().map(it -> it.startLogin(userName, password))
        .orElseGet(() -> PendingResult.completedWith(BBjLoginStatus.UNAVAILABLE));
  }

  /**
   * Signs in the user whose credentials a {@link Login} component submitted.
   *
   * <p>
   * The component shows no error while the sign in runs. When the credentials are invalid the
   * component shows its error state. When the sign in is unavailable a warning toast is shown.
   * After a successful sign in the component is closed, otherwise it is enabled again. The sign in
   * is cancelled when the component is destroyed before it completes.
   * </p>
   *
   * @param event the submit event of the component
   *
   * @return the pending status of the sign in
   * @throws NullPointerException if {@code event} is {@code null}
   * @throws ClassCastException if the source of the event is not a {@link Login}
   */
  public static PendingResult<BBjLoginStatus> login(LoginSubmitEvent event) {
    Objects.requireNonNull(event, "event");

    Login login = (Login) event.getComponent();
    login.setError(false);
    PendingResult<BBjLoginStatus> result;
    try {
      result = login(event.getUsername(), event.getPassword());
    } catch (RuntimeException e) {
      login.setEnabled(true);
      throw e;
    }

    ComponentLifecycleObserver observer = (component, lifecycleEvent) -> {
      if (lifecycleEvent == LifecycleEvent.DESTROY) {
        result.cancel();
      }
    };
    login.addLifecycleObserver(observer);
    result.thenAccept(status -> {
      login.removeLifecycleObserver(observer);
      if (status == BBjLoginStatus.INVALID_CREDENTIALS) {
        login.setError(true);
      } else if (status == BBjLoginStatus.UNAVAILABLE) {
        Toast.show(UNAVAILABLE_TEXT, Theme.WARNING);
      }

      if (status == BBjLoginStatus.AUTHENTICATED) {
        login.close();
      } else {
        login.setEnabled(true);
      }
    });

    return result;
  }

  /**
   * Gets the user that signed in.
   *
   * @return the signed in user, or empty if nobody has signed in or BBj security is not set up
   */
  public static Optional<BBjPrincipal> currentUser() {
    return getService().flatMap(BBjAuthenticationService::currentUser);
  }

  private static Optional<BBjAuthenticationService> getService() {
    if (ObjectTable.contains(SERVICE_KEY)) {
      return Optional.of((BBjAuthenticationService) ObjectTable.get(SERVICE_KEY));
    }

    return Optional.empty();
  }
}
