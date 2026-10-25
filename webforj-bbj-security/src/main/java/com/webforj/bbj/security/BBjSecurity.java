package com.webforj.bbj.security;

import com.webforj.PendingResult;
import com.webforj.bbj.security.model.BBjPrincipal;
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
 * BBjSecurity
 */
public final class BBjSecurity {
  public static final String SERVICE_KEY = BBjAuthenticationService.class.getName();
  private static final String UNAVAILABLE_TEXT = "Cannot sign in right now.";

  private BBjSecurity() {}

  public static PendingResult<BBjLoginStatus> login(String userName, String password) {
    Objects.requireNonNull(userName, "userName");
    Objects.requireNonNull(password, "password");

    return getService().map(it -> it.startLogin(userName, password))
        .orElseGet(() -> PendingResult.completedWith(BBjLoginStatus.UNAVAILABLE));
  }

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
