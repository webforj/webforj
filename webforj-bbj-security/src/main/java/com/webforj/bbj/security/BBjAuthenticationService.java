package com.webforj.bbj.security;

import com.webforj.bbj.security.BBjAdminFailure.Kind;
import com.webforj.bbj.security.model.BBjLoginStatus;
import com.webforj.bbj.security.model.BBjPrincipal;
import com.webforj.bbj.security.model.BBjSession;
import com.webforj.bbj.security.store.BBjSecurityStore;
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
  private final BBjAdminGateway gateway;
  private final BBjSecurityStore store;
  private final BBjSecurityConfigurationProperties properties;
  private final BBjRouteSecurityConfiguration configuration;
  private final Consumer<Location> navigator;
  private final Clock clock;
  private BBjSession session;
  private Instant lastCheck;

  /**
   * Creates a service from the values of a builder.
   *
   * @param builder the builder that holds the values of the service
   */
  private BBjAuthenticationService(Builder builder) {
    gateway = builder.gateway;
    store = builder.store;
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
   * Signs a user in.
   *
   * <p>
   * The sign in runs on the calling thread. The credentials are passed to BBj as given. After a
   * successful sign in the session is saved in the session store. The sign in opens no location.
   * </p>
   *
   * <p>
   * When the session cannot be saved, the sign in fails as unavailable, its login token is ended
   * and the user that was signed in before stays signed in.
   * </p>
   *
   * <p>
   * When the account needs a 2 factor code that was not given, or the given code is refused, the
   * sign in ends as {@link BBjLoginStatus#TWO_FACTOR_REQUIRED}.
   * </p>
   *
   * @param userName the user name
   * @param password the password
   * @param code the 2 factor code, or {@code null} to sign in with the password only
   *
   * @return the status of the sign in
   * @throws NullPointerException if {@code userName} or {@code password} is {@code null}
   */
  BBjLoginStatus login(String userName, String password, String code) {
    Objects.requireNonNull(userName, "userName");
    Objects.requireNonNull(password, "password");

    BBjSession opened;
    try {
      opened = gateway.openSession(userName, password, code);
    } catch (BBjAdminFailure e) {
      return applyFailure(e);
    } catch (RuntimeException e) {
      return applyFailure(createUnavailableFailure());
    }

    return applySession(opened);
  }

  /**
   * Signs the current user out.
   *
   * <p>
   * When nobody has signed in, nothing happens. Otherwise the user is signed out at once, the saved
   * session is cleared, the requested location is cleared and the service opens the logout
   * location. Then the login token is ended. A failure to end it is logged and the token expires on
   * its own.
   * </p>
   */
  void logout() {
    if (session == null) {
      return;
    }

    final var current = session;
    session = null;
    store.clearSession();
    store.clearLocation();
    openLogoutLocation();
    closeSession(current);
  }

  /**
   * Checks the signed in user against the session store and the server.
   *
   * <p>
   * The session store is read first, because all tabs of one browser share it. When it holds no
   * session, the check ends there. A session held by this service is then dropped without a call to
   * the server, without a change to the session store and without opening a location. When the
   * store holds a session with another token than this service, or this service holds none, the
   * stored session takes its place and its check is due at once.
   * </p>
   *
   * <p>
   * A session that has reached its expiry is cleared at once without a call to the server. A check
   * that is not due yet does nothing. A check is due when the session was not checked since the
   * sign in, the restore or the change in the session store, when {@code check-seconds} have passed
   * since the last check, and while the session is inside the last {@code renew-before-minutes}
   * before its expiry.
   * </p>
   *
   * <p>
   * A due check runs on the calling thread. When the server no longer accepts the token or knows it
   * for another user, the session store is read again. A session with another token found there
   * takes the place of the checked one and the session store is kept. Otherwise the user is signed
   * out. Inside the renew window the token is renewed and the renewed session is saved in the
   * session store. When the renewed session cannot be saved, the user is signed out and the renewed
   * token is ended. Any other failure keeps the user signed in until the session expires, and the
   * next check is due after {@code check-seconds} or, inside the renew window, at the next call.
   * </p>
   */
  void check() {
    Optional<BBjSession> stored = store.readSession();
    if (stored.isEmpty()) {
      applyMissingSession();
      return;
    }

    applyStoredSession(stored.get());
    final var checked = session;
    var now = clock.instant();
    if (!now.isBefore(checked.getExpiresAt())) {
      clearSignIn();
      logger.log(Level.DEBUG, "The sign in expired and was cleared");

      return;
    }

    if (!isCheckDue(checked, now)) {
      return;
    }

    BBjSession outcome;
    try {
      if (!checked.getPrincipal().getUserName().equals(gateway.readUserName(checked))) {
        throw new BBjAdminFailure(Kind.TOKEN_REJECTED,
            "The server knows the token for another user");
      }

      outcome = isInsideRenewalWindow(checked, now) ? gateway.renewSession(checked) : checked;
    } catch (BBjAdminFailure e) {
      applyCheckFailure(checked, e);
      return;
    } catch (RuntimeException e) {
      applyCheckFailure(checked, createUnavailableFailure());
      return;
    }

    applyCheck(checked, outcome);
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
   * The session saved in the session store is put in place without calling BBj. When the store
   * holds no usable session, nobody is signed in.
   * </p>
   */
  void restoreSession() {
    store.readSession().ifPresent(it -> session = it);
  }

  /**
   * Opens the location the user lands on after signing in.
   *
   * <p>
   * The location the user requested before signing in is opened, or the default success path when
   * no location was requested or returning is switched off. The requested location is cleared
   * either way.
   * </p>
   */
  void openTargetLocation() {
    Optional<Location> requested = store.readLocation();
    store.clearLocation();
    if (Boolean.TRUE.equals(properties.getReturnAfterLogin()) && requested.isPresent()) {
      navigator.accept(requested.get());
    } else {
      navigator.accept(new Location(properties.getDefaultSuccessPath()));
    }
  }

  private BBjLoginStatus applySession(BBjSession session) {
    try {
      store.saveSession(session);
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
      case TWO_FACTOR_REQUIRED, INVALID_CODE -> BBjLoginStatus.TWO_FACTOR_REQUIRED;
      case TOKEN_REJECTED, UNAVAILABLE -> {
        logger.log(Level.WARNING, "The BBj Admin API could not complete the sign in: {0}",
            failure.getMessage());

        yield BBjLoginStatus.UNAVAILABLE;
      }
    };
  }

  private void applyCheck(BBjSession checked, BBjSession outcome) {
    if (outcome != checked) {
      applyRenewal(outcome);
    } else {
      lastCheck = clock.instant();
      logger.log(Level.DEBUG, "The sign in was checked and stays");
    }
  }

  private void applyMissingSession() {
    if (session != null) {
      session = null;
      logger.log(Level.DEBUG, "The session store holds no sign in, the sign in was dropped");
    }
  }

  private void applyStoredSession(BBjSession stored) {
    if (session == null || !session.getToken().equals(stored.getToken())) {
      session = stored;
      lastCheck = null;
    }
  }

  private void applyCheckFailure(BBjSession checked, BBjAdminFailure failure) {
    if (failure.getKind() == Kind.TOKEN_REJECTED) {
      applyRejection(checked);
    } else {
      lastCheck = clock.instant();
      logger.log(Level.WARNING, "The sign in could not be checked and stays until it expires: {0}",
          failure.getMessage());
    }
  }

  private void applyRejection(BBjSession checked) {
    Optional<BBjSession> stored = store.readSession();
    if (stored.isPresent() && !stored.get().getToken().equals(checked.getToken())) {
      applyStoredSession(stored.get());
      logger.log(Level.DEBUG, "Another sign in in the session store replaced the rejected one");
      return;
    }

    clearSignIn();
    logger.log(Level.INFO, "The server no longer accepts the sign in, the user was signed out");
  }

  private void applyRenewal(BBjSession renewed) {
    try {
      store.saveSession(renewed);
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
    store.clearSession();
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

  private void closeSession(BBjSession session) {
    try {
      gateway.closeSession(session);
    } catch (BBjAdminFailure e) {
      applyCloseFailure(e);
    } catch (RuntimeException e) {
      applyCloseFailure(createUnavailableFailure());
    }
  }

  private void applyCloseFailure(BBjAdminFailure failure) {
    var kind = failure.getKind();
    if (kind == Kind.TOKEN_REJECTED) {
      logger.log(Level.DEBUG, "The token was already ended: {0}", failure.getMessage());
    } else {
      logger.log(Level.WARNING, "The token could not be ended and expires on its own: {0}",
          failure.getMessage());
    }
  }

  private void openLogoutLocation() {
    configuration.getLogoutLocation().ifPresent(navigator::accept);
  }

  private static BBjAdminFailure createUnavailableFailure() {
    return new BBjAdminFailure(Kind.UNAVAILABLE, "The Admin API call could not be completed");
  }

  /**
   * Builds a {@link BBjAuthenticationService}.
   *
   * @author Christian Liebhardt
   * @since 26.03
   */
  public static final class Builder {
    private BBjAdminGateway gateway;
    private BBjSecurityStore store;
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
      Objects.requireNonNull(store, "store");
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
     * Sets the store of the service.
     *
     * @param store keeps the session of the signed in user and the location requested before
     *
     * @return this builder
     */
    public Builder setStore(BBjSecurityStore store) {
      this.store = store;
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
