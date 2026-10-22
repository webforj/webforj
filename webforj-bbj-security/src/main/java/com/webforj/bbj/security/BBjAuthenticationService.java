package com.webforj.bbj.security;

import com.webforj.PendingResult;
import com.webforj.bbj.security.model.BBjLoginStatus;
import com.webforj.router.history.Location;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Consumer;
import java.util.function.Supplier;

/**
 * Signs visitors in against the BBj Admin API.
 *
 * <p>
 * A sign-in runs on a worker thread. Its result is delivered on the Environment thread, where the
 * session is kept and the authentication state is filled. The visitor is sent to the requested
 * location after the result is completed, so a view that cancels its result when the navigation
 * destroys it finds nothing left to cancel. No message of this class carries a password, a token or
 * the text of an exception.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
final class BBjAuthenticationService {
  private static final Logger logger = System.getLogger(BBjAuthenticationService.class.getName());
  private static final Location DEFAULT_LOCATION = new Location("/");

  private final BBjAdminGateway gateway;
  private final BBjAdminTaskRunner runner;
  private final BBjTokenStore store;
  private final BBjAuthenticationState state;
  private final Supplier<Optional<Location>> requestedLocation;
  private final Consumer<Location> navigator;

  /**
   * Creates a service.
   *
   * @param gateway the calls to the Admin API
   * @param runner runs the blocking calls off the UI thread
   * @param store keeps the session of the visitor
   * @param state the authentication state to fill
   * @param requestedLocation gives and clears the location the visitor asked for
   * @param navigator sends the visitor to a location
   *
   * @throws NullPointerException if an argument is {@code null}
   */
  BBjAuthenticationService(BBjAdminGateway gateway, BBjAdminTaskRunner runner, BBjTokenStore store,
      BBjAuthenticationState state, Supplier<Optional<Location>> requestedLocation,
      Consumer<Location> navigator) {
    this.gateway = Objects.requireNonNull(gateway, "gateway");
    this.runner = Objects.requireNonNull(runner, "runner");
    this.store = Objects.requireNonNull(store, "store");
    this.state = Objects.requireNonNull(state, "state");
    this.requestedLocation = Objects.requireNonNull(requestedLocation, "requestedLocation");
    this.navigator = Objects.requireNonNull(navigator, "navigator");
  }

  /**
   * Starts a sign-in on the Environment thread.
   *
   * <p>
   * A blank username or password answers {@link BBjLoginStatus#INVALID_CREDENTIALS} without a
   * server call. The result never completes exceptionally. Cancelling it before delivery keeps the
   * visitor signed out and retires a session that arrived anyway.
   * </p>
   *
   * @param userName the BBj username
   * @param password the password of the user
   * @return the result, completed on the Environment thread
   *
   * @throws NullPointerException if an argument is {@code null}
   */
  PendingResult<BBjLoginStatus> startLogin(String userName, String password) {
    Objects.requireNonNull(userName, "userName");
    Objects.requireNonNull(password, "password");
    if (userName.isBlank() || password.isBlank()) {
      return PendingResult.completedWith(BBjLoginStatus.INVALID_CREDENTIALS);
    }

    PendingResult<BBjLoginStatus> result =
        runner.start(() -> gateway.openSession(userName, password), this::applySession,
            this::applyFailure, this::closeLateSession);
    result.thenAccept(status -> {
      if (status == BBjLoginStatus.AUTHENTICATED) {
        openTargetLocation();
      }
    });

    return result;
  }

  private BBjLoginStatus applySession(BBjLoginSession session) {
    try {
      store.saveSession(session);
    } catch (RuntimeException e) {
      logger.log(Level.WARNING, "The session could not be kept, sign-in refused with {0}",
          e.getClass().getName());
      closeUnkeptSession(session);

      return BBjLoginStatus.UNAVAILABLE;
    }

    state.setAuthentication(
        new BBjAuthentication(session.getUserName(), Set.of(), session.getExpiresAt()));

    return BBjLoginStatus.AUTHENTICATED;
  }

  private BBjLoginStatus applyFailure(BBjAdminFailure failure) {
    return switch (failure.getKind()) {
      case INVALID_CREDENTIALS -> BBjLoginStatus.INVALID_CREDENTIALS;
      case TWO_FACTOR_REQUIRED -> {
        logger.log(Level.WARNING,
                "Two-factor authentication is required for this account and is not supported yet");
        yield BBjLoginStatus.UNAVAILABLE;
      }
      default -> {
        logger.log(Level.WARNING, "The BBj Admin API could not complete the sign-in");
        yield BBjLoginStatus.UNAVAILABLE;
      }
    };
  }

  private void openTargetLocation() {
    try {
      navigator.accept(requestedLocation.get().orElse(DEFAULT_LOCATION));
    } catch (RuntimeException e) {
      logger.log(Level.WARNING, "Navigation after sign-in failed with {0}", e.getClass().getName());
    }
  }

  private void closeUnkeptSession(BBjLoginSession session) {
    runner.start(() -> {
      gateway.closeSession(session);

      return Boolean.TRUE;
    }, closed -> closed, failure -> {
      logger.log(Level.WARNING, "A session that was not kept could not be closed on the server");

      return Boolean.FALSE;
    }, closed -> {
    });
  }

  private void closeLateSession(BBjLoginSession session) {
    try {
      gateway.closeSession(session);
    } catch (RuntimeException e) {
      logger.log(Level.WARNING, "A session nobody took could not be closed on the server");
    }
  }
}
