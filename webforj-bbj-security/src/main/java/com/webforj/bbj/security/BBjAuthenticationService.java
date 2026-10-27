package com.webforj.bbj.security;

import com.webforj.PendingResult;
import com.webforj.bbj.security.BBjAdminFailure.Kind;
import com.webforj.bbj.security.model.BBjLoginStatus;
import com.webforj.bbj.security.model.BBjPrincipal;
import com.webforj.bbj.security.model.BBjSession;
import com.webforj.router.history.Location;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Consumer;

/**
 * Signs users in against BBj and keeps the user that signed in.
 *
 * <p>
 * The service is registered in the object table of the environment under
 * {@link BBjSecurity#SERVICE_KEY}. Apps use {@link BBjSecurity} to reach it.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
public final class BBjAuthenticationService {
  private static final Logger logger = System.getLogger(BBjAuthenticationService.class.getName());
  private static final int MAX_USER_NAME_LENGTH = 256;
  private static final int MAX_PASSWORD_LENGTH = 1024;
  private final BBjAdminGateway gateway;
  private final BBjAdminWorker worker;
  private final BBjRequestedLocationStore locationStore;
  private final BBjSessionStore sessionStore;
  private final BBjSecurityConfigurationProperties properties;
  private final BBjRouteSecurityConfiguration configuration;
  private final Consumer<Location> navigator;
  private final Clock clock;
  private BBjSession session;
  private PendingResult<BBjLoginStatus> pendingLogin;
  private PendingResult<Void> pendingCheck;
  private Instant lastCheck;

  /**
   * Creates a service from the values of a builder.
   *
   * @param builder the builder that holds the values of the service
   */
  private BBjAuthenticationService(Builder builder) {
    gateway = builder.gateway;
    worker = builder.worker;
    locationStore = builder.locationStore;
    sessionStore = builder.sessionStore;
    properties = builder.properties;
    configuration = builder.configuration;
    navigator = builder.navigator;
    clock = builder.clock;
  }

  /**
   * Creates a builder for a service.
   *
   * @return a new builder
   */
  static Builder builder() {
    return new Builder();
  }

  /**
   * Starts signing a user in.
   *
   * <p>
   * An empty or blank user name or password, a user name longer than 256 characters and a password
   * longer than 1024 characters are refused at once as invalid credentials. Otherwise a sign in
   * that is still running is cancelled and the new sign in runs in the background. After a
   * successful sign in the session is saved in the session store and the service opens the location
   * the user requested before signing in, or the default success path when no location was
   * requested or returning is switched off. The requested location is cleared either way.
   * </p>
   *
   * <p>
   * When the session cannot be saved, the sign in fails as unavailable, its login token is ended
   * and the user that was signed in before stays signed in.
   * </p>
   *
   * @param userName the user name
   * @param password the password
   *
   * @return the pending status of the sign in
   * @throws NullPointerException if any parameter is {@code null}
   */
  PendingResult<BBjLoginStatus> startLogin(String userName, String password) {
    Objects.requireNonNull(userName, "userName");
    Objects.requireNonNull(password, "password");
    if (userName.isBlank() || password.isBlank() || userName.length() > MAX_USER_NAME_LENGTH
        || password.length() > MAX_PASSWORD_LENGTH) {
      return PendingResult.completedWith(BBjLoginStatus.INVALID_CREDENTIALS);
    }

    stopPendingLogin();
    PendingResult<BBjLoginStatus> result =
        worker.start(() -> gateway.openSession(userName, password), this::applySession,
            this::applyFailure, this::discardLateSession);
    pendingLogin = result;
    result.thenAccept(status -> {
      pendingLogin = null;
      if (status == BBjLoginStatus.AUTHENTICATED) {
        openTargetLocation();
      }
    });

    return result;
  }

  /**
   * Starts signing the current user out.
   *
   * <p>
   * A sign in that is still running is cancelled and never keeps a session. When nobody has signed
   * in, the pending result is complete and nothing else happens. Otherwise the user is signed out
   * at once, the saved session is cleared, the requested location is cleared and the service opens
   * the logout location. The login token is ended in the background and the pending result
   * completes when that ends, also when ending the token fails.
   * </p>
   *
   * @return the pending end of the sign out
   */
  PendingResult<Void> startLogout() {
    stopPendingLogin();
    if (session == null) {
      return PendingResult.completedWith(null);
    }
    final var current = session;
    session = null;
    sessionStore.clearSession();
    locationStore.clearLocation();
    openLogoutLocation();
    return closeSession(current);
  }

  /**
   * Starts checking the signed in user against the server.
   *
   * <p>
   * When nobody has signed in, the pending result is complete and nothing else happens. A session
   * that has reached its expiry is cleared at once without a call to the server. A check that is
   * still running is returned to every caller. A check that is not due yet completes at once. A
   * check is due when the session was not checked since the sign in or the restore, when
   * {@code check-seconds} have passed since the last check, and while the session is inside the
   * last {@code renew-before-minutes} before its expiry.
   * </p>
   *
   * <p>
   * A due check runs in the background. It signs the user out when the server no longer accepts the
   * token or knows it for another user. Inside the renew window the token is renewed and the
   * renewed session is saved in the session store. When the renewed session cannot be saved, the
   * user is signed out and the renewed token is ended. Any other failure keeps the user signed in
   * until the session expires, and the next check is due after {@code check-seconds} or, inside the
   * renew window, at the next call. A result for a session that is no longer the current one is
   * dropped, and a renewed token in it is ended.
   * </p>
   *
   * @return the pending end of the check
   */
  PendingResult<Void> startCheck() {
    if (session == null) {
      return PendingResult.completedWith(null);
    }

    final var checked = session;
    var now = clock.instant();
    if (!now.isBefore(checked.getExpiresAt())) {
      clearSignIn();
      logger.log(Level.DEBUG, "The sign in expired and was cleared");

      return PendingResult.completedWith(null);
    }

    if (pendingCheck != null && !pendingCheck.isDone()) {
      return pendingCheck;
    }

    if (!isCheckDue(checked, now)) {
      return PendingResult.completedWith(null);
    }

    final var renewing = isInsideRenewalWindow(checked, now);
    pendingCheck = worker.start(() -> {
      if (!checked.getPrincipal().getUserName().equals(gateway.readUserName(checked))) {
        throw new BBjAdminFailure(Kind.TOKEN_REJECTED,
            "The server knows the token for another user");
      }

      return renewing ? gateway.renewSession(checked) : checked;
    }, outcome -> applyCheck(checked, outcome), failure -> applyCheckFailure(checked, failure),
        outcome -> {
          if (outcome != checked) {
            discardLateSession(outcome);
          }
        });

    return pendingCheck;
  }

  /**
   * Gets the user that signed in.
   *
   * @return the signed in user, or empty if nobody has signed in
   */
  Optional<BBjPrincipal> currentUser() {
    return Optional.ofNullable(session).map(BBjSession::getPrincipal);
  }

  /**
   * Restores the user that signed in before the page was reloaded.
   *
   * <p>
   * The session saved for the current tab is put in place without calling BBj. When the store holds
   * no usable session, nobody is signed in.
   * </p>
   */
  void restoreSession() {
    sessionStore.readSession().ifPresent(it -> session = it);
  }

  private BBjLoginStatus applySession(BBjSession session) {
    try {
      sessionStore.saveSession(session);
    } catch (IllegalStateException e) {
      closeSession(session);
      return BBjLoginStatus.UNAVAILABLE;
    }

    var replaced = this.session;
    this.session = session;
    lastCheck = clock.instant();
    if (replaced != null) {
      closeSession(replaced);
    }

    return BBjLoginStatus.AUTHENTICATED;
  }

  private BBjLoginStatus applyFailure(BBjAdminFailure failure) {
    return switch (failure.getKind()) {
      case INVALID_CREDENTIALS -> BBjLoginStatus.INVALID_CREDENTIALS;
      case TWO_FACTOR_REQUIRED -> {
        logger.log(Level.WARNING,
            "Two factor authentication is required for this account and is not supported yet");

        yield BBjLoginStatus.UNAVAILABLE;
      }
      case TOKEN_REJECTED, UNAVAILABLE -> {
        logger.log(Level.WARNING, "The BBj Admin API could not complete the sign in: {0}",
            failure.getMessage());

        yield BBjLoginStatus.UNAVAILABLE;
      }
    };
  }

  private Void applyCheck(BBjSession checked, BBjSession outcome) {
    var renewed = outcome != checked;
    if (!isCurrent(checked)) {
      if (renewed) {
        closeSession(outcome);
      }

      return null;
    }

    if (renewed) {
      applyRenewal(outcome);
    } else {
      lastCheck = clock.instant();
      logger.log(Level.DEBUG, "The sign in was checked and stays");
    }

    return null;
  }

  private Void applyCheckFailure(BBjSession checked, BBjAdminFailure failure) {
    if (!isCurrent(checked)) {
      return null;
    }

    if (failure.getKind() == Kind.TOKEN_REJECTED) {
      clearSignIn();
      logger.log(Level.INFO, "The server no longer accepts the sign in, the user was signed out");
    } else {
      lastCheck = clock.instant();
      logger.log(Level.WARNING, "The sign in could not be checked and stays until it expires: {0}",
          failure.getMessage());
    }

    return null;
  }

  private void applyRenewal(BBjSession renewed) {
    try {
      sessionStore.saveSession(renewed);
    } catch (IllegalStateException e) {
      logger.log(Level.WARNING, "The renewed sign in could not be kept, the user was signed out");
      clearSignIn();
      closeSession(renewed);

      return;
    }

    session = renewed;
    lastCheck = clock.instant();
    logger.log(Level.INFO, "The sign in was renewed");
  }

  private void clearSignIn() {
    session = null;
    sessionStore.clearSession();
  }

  private boolean isCurrent(BBjSession checked) {
    return session == checked;
  }

  private boolean isCheckDue(BBjSession checked, Instant now) {
    return lastCheck == null
        || !now.isBefore(lastCheck.plus(Duration.ofSeconds(properties.getCheckSeconds())))
        || isInsideRenewalWindow(checked, now);
  }

  private boolean isInsideRenewalWindow(BBjSession checked, Instant now) {
    var minutes = Duration.ofMinutes(properties.getRenewBeforeMinutes());
    return !now.plus(minutes).isBefore(checked.getExpiresAt());
  }

  private void stopPendingLogin() {
    if (pendingLogin != null) {
      pendingLogin.cancel();
      pendingLogin = null;
    }
  }

  private PendingResult<Void> closeSession(BBjSession session) {
    return worker.start(() -> {
      gateway.closeSession(session);
      return Boolean.TRUE;
    }, closed -> null, this::applyCloseFailure, discard -> discardLateSession(session));
  }

  private Void applyCloseFailure(BBjAdminFailure failure) {
    var kind = failure.getKind();
    if (kind == Kind.TOKEN_REJECTED) {
      logger.log(Level.DEBUG, "The token was already ended: {0}", failure.getMessage());
    } else {
      logger.log(Level.WARNING, "The token could not be ended and expires on its own: {0}",
          failure.getMessage());
    }
    return null;
  }

  private void openTargetLocation() {
    Optional<Location> requested = locationStore.readLocation();
    locationStore.clearLocation();
    if (Boolean.TRUE.equals(properties.getReturnAfterLogin()) && requested.isPresent()) {
      navigator.accept(requested.get());
    } else {
      navigator.accept(new Location(properties.getDefaultSuccessPath()));
    }
  }

  private void openLogoutLocation() {
    configuration.getLogoutLocation().ifPresent(navigator::accept);
  }

  private void discardLateSession(BBjSession session) {
    gateway.closeSession(session);
  }

  /**
   * Builds a {@link BBjAuthenticationService}.
   *
   * @author Christian Liebhardt
   * @since 26.03
   */
  public static final class Builder {
    private BBjAdminGateway gateway;
    private BBjAdminWorker worker;
    private BBjRequestedLocationStore locationStore;
    private BBjSessionStore sessionStore;
    private BBjSecurityConfigurationProperties properties;
    private BBjRouteSecurityConfiguration configuration;
    private Consumer<Location> navigator;
    private Clock clock;

    /**
     * Builds the service.
     *
     * @return the service
     * @throws NullPointerException if a value was not set, naming the missing value
     */
    public BBjAuthenticationService build() {
      Objects.requireNonNull(gateway, "gateway");
      Objects.requireNonNull(worker, "worker");
      Objects.requireNonNull(locationStore, "locationStore");
      Objects.requireNonNull(sessionStore, "sessionStore");
      Objects.requireNonNull(properties, "properties");
      Objects.requireNonNull(configuration, "configuration");
      Objects.requireNonNull(navigator, "navigator");
      if (clock == null) {
        clock = Clock.systemUTC();
      }
      return new BBjAuthenticationService(this);
    }

    /**
     * Sets the gateway of the service.
     *
     * @param gateway opens and closes the sessions
     *
     * @return this builder
     */
    public Builder setGateway(BBjAdminGateway gateway) {
      this.gateway = gateway;
      return this;
    }

    /**
     * Sets the worker of the service.
     *
     * @param worker runs the sign in away from the calling thread
     *
     * @return this builder
     */
    public Builder setWorker(BBjAdminWorker worker) {
      this.worker = worker;
      return this;
    }

    /**
     * Sets the location store of the service.
     *
     * @param locationStore holds the location the user requested before signing in
     *
     * @return this builder
     */
    public Builder setLocationStore(BBjRequestedLocationStore locationStore) {
      this.locationStore = locationStore;
      return this;
    }

    /**
     * Sets the session store of the service.
     *
     * @param sessionStore keeps the session of the signed in user across a page reload
     *
     * @return this builder
     */
    public Builder setSessionStore(BBjSessionStore sessionStore) {
      this.sessionStore = sessionStore;
      return this;
    }

    /**
     * Sets the properties of the service.
     *
     * @param properties the configuration of the sign in
     *
     * @return this builder
     */
    public Builder setProperties(BBjSecurityConfigurationProperties properties) {
      this.properties = properties;
      return this;
    }

    /**
     * Sets the configuration of the service.
     *
     * @param configuration the route security configuration that gives the logout location
     *
     * @return this builder
     */
    public Builder setConfiguration(BBjRouteSecurityConfiguration configuration) {
      this.configuration = configuration;
      return this;
    }

    /**
     * Sets the navigator of the service.
     *
     * @param navigator opens the location the user lands on after signing in
     *
     * @return this builder
     */
    public Builder setNavigator(Consumer<Location> navigator) {
      this.navigator = navigator;
      return this;
    }

    /**
     * Sets the clock of the service.
     *
     * @param clock tells the time that expiry, checks and renewals are measured against
     *
     * @return this builder
     */
    public Builder setClock(Clock clock) {
      this.clock = clock;
      return this;
    }
  }
}
