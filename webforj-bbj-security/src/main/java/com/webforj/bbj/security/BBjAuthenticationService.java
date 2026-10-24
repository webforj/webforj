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
 * <p>
 * A sign-out clears the authentication state and the kept session first, sends the visitor to the
 * logout location and ends the token on the server last, on a worker thread. The visitor is signed
 * out even when the server cannot be reached.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
final class BBjAuthenticationService {
  private static final Logger logger = System.getLogger(BBjAuthenticationService.class.getName());

  private final BBjAdminGateway gateway;
  private final BBjAdminTaskRunner runner;
  private final BBjTokenStore store;
  private final BBjAuthenticationState state;
  private final Supplier<Optional<Location>> requestedLocation;
  private final Consumer<Location> navigator;
  private final BBjRouteSecurityConfiguration configuration;
  private PendingResult<BBjLoginStatus> loginInFlight;

  private BBjAuthenticationService(Builder builder) {
    this.gateway = Objects.requireNonNull(builder.gateway, "gateway");
    this.runner = Objects.requireNonNull(builder.runner, "runner");
    this.store = Objects.requireNonNull(builder.store, "store");
    this.state = Objects.requireNonNull(builder.state, "state");
    this.requestedLocation = Objects.requireNonNull(builder.requestedLocation, "requestedLocation");
    this.navigator = Objects.requireNonNull(builder.navigator, "navigator");
    this.configuration = Objects.requireNonNull(builder.configuration, "configuration");
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
    loginInFlight = result;

    return result;
  }

  /**
   * Signs the visitor out on the Environment thread.
   *
   * <p>
   * A sign-in in flight is cancelled, the authentication state and the kept session are cleared,
   * the requested location is dropped and, when a sign-in was held, the visitor is sent to the
   * logout location. The token is ended on the server last. The result completes after that call
   * ended, or at once when there is no token to end. It never completes exceptionally.
   * </p>
   *
   * <p>
   * A visitor with no authentication and no kept session stays where they are and no server call is
   * made.
   * </p>
   *
   * @return the result, completed on the Environment thread
   */
  PendingResult<Void> startLogout() {
    if (loginInFlight != null) {
      loginInFlight.cancel();
      loginInFlight = null;
    }

    boolean signedIn = state.getAuthentication().isPresent();
    state.clearAuthentication();
    Optional<BBjLoginSession> stored = readStoredSession();
    applyStep(store::clearSession);
    applyStep(requestedLocation::get);
    if (!signedIn && stored.isEmpty()) {
      return PendingResult.completedWith(null);
    }

    if (signedIn) {
      openLogoutLocation();
    }

    if (stored.isEmpty()) {
      return PendingResult.completedWith(null);
    }

    return closeSession(stored.get(),
        "The BBj login token could not be ended and expires on its own");
  }

  /**
   * Starts building a service.
   *
   * @return a builder with no collaborator set
   */
  static Builder create() {
    return new Builder();
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
      Optional<Location> requested = requestedLocation.get();
      navigator.accept(configuration.isReturnAfterLogin()
          ? requested.orElseGet(configuration::getDefaultSuccessLocation)
          : configuration.getDefaultSuccessLocation());
    } catch (RuntimeException e) {
      logger.log(Level.WARNING, "Navigation after sign-in failed with {0}", e.getClass().getName());
    }
  }

  private void openLogoutLocation() {
    try {
      navigator.accept(configuration.getLogoutLocation());
    } catch (RuntimeException e) {
      logger.log(Level.WARNING, "Navigation after sign-out failed with {0}",
          e.getClass().getName());
    }
  }

  private Optional<BBjLoginSession> readStoredSession() {
    try {
      return store.readSession();
    } catch (RuntimeException e) {
      logger.log(Level.WARNING, "The sign-out step failed with {0}", e.getClass().getName());

      return Optional.empty();
    }
  }

  private void applyStep(Runnable step) {
    try {
      step.run();
    } catch (RuntimeException e) {
      logger.log(Level.WARNING, "The sign-out step failed with {0}", e.getClass().getName());
    }
  }

  private void closeUnkeptSession(BBjLoginSession session) {
    closeSession(session, "A session that was not kept could not be closed on the server");
  }

  private PendingResult<Void> closeSession(BBjLoginSession session, String failureText) {
    return runner.start(() -> {
      gateway.closeSession(session);

      return Boolean.TRUE;
    }, closed -> null, failure -> {
      if (failure.getKind() == BBjAdminFailure.Kind.TOKEN_REJECTED) {
        logger.log(Level.DEBUG, "The BBj login token was already ended");
      } else {
        logger.log(Level.WARNING, failureText);
      }

      return null;
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

  /**
   * Collects the collaborators of a {@link BBjAuthenticationService}.
   */
  static final class Builder {
    BBjAdminGateway gateway;
    BBjAdminTaskRunner runner;
    BBjTokenStore store;
    BBjAuthenticationState state;
    Supplier<Optional<Location>> requestedLocation;
    Consumer<Location> navigator;
    BBjRouteSecurityConfiguration configuration;

    private Builder() {}

    /**
     * Sets the gateway.
     *
     * @param gateway the calls to the Admin API
     * @return this builder
     *
     * @throws NullPointerException if the argument is {@code null}
     */
    Builder setGateway(BBjAdminGateway gateway) {
      this.gateway = Objects.requireNonNull(gateway, "gateway");

      return this;
    }

    /**
     * Sets the runner.
     *
     * @param runner runs the blocking calls off the UI thread
     * @return this builder
     *
     * @throws NullPointerException if the argument is {@code null}
     */
    Builder setRunner(BBjAdminTaskRunner runner) {
      this.runner = Objects.requireNonNull(runner, "runner");

      return this;
    }

    /**
     * Sets the store.
     *
     * @param store keeps the session of the visitor
     * @return this builder
     *
     * @throws NullPointerException if the argument is {@code null}
     */
    Builder setStore(BBjTokenStore store) {
      this.store = Objects.requireNonNull(store, "store");

      return this;
    }

    /**
     * Sets the state.
     *
     * @param state the authentication state to fill
     * @return this builder
     *
     * @throws NullPointerException if the argument is {@code null}
     */
    Builder setState(BBjAuthenticationState state) {
      this.state = Objects.requireNonNull(state, "state");

      return this;
    }

    /**
     * Sets the requestedLocation.
     *
     * @param requestedLocation gives and clears the location the visitor asked for
     * @return this builder
     *
     * @throws NullPointerException if the argument is {@code null}
     */
    Builder setRequestedLocation(Supplier<Optional<Location>> requestedLocation) {
      this.requestedLocation = Objects.requireNonNull(requestedLocation, "requestedLocation");

      return this;
    }

    /**
     * Sets the navigator.
     *
     * @param navigator sends the visitor to a location
     * @return this builder
     *
     * @throws NullPointerException if the argument is {@code null}
     */
    Builder setNavigator(Consumer<Location> navigator) {
      this.navigator = Objects.requireNonNull(navigator, "navigator");

      return this;
    }

    /**
     * Sets the configuration.
     *
     * @param configuration the navigation settings
     * @return this builder
     *
     * @throws NullPointerException if the argument is {@code null}
     */
    Builder setConfiguration(BBjRouteSecurityConfiguration configuration) {
      this.configuration = Objects.requireNonNull(configuration, "configuration");

      return this;
    }

    /**
     * Builds the service.
     *
     * @return the service
     *
     * @throws NullPointerException if a collaborator was not set
     */
    BBjAuthenticationService build() {
      return new BBjAuthenticationService(this);
    }
  }
}
