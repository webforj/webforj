package com.webforj.bbj.security;

import com.webforj.bbj.security.BBjAdminFailure.Kind;
import com.webforj.bbj.security.model.BBjLoginStatus;
import com.webforj.bbj.security.model.BBjPrincipal;
import com.webforj.bbj.security.model.BBjSession;
import com.webforj.bbj.security.store.BBjSecurityStore;
import com.webforj.bbj.security.store.BBjThrottleStore;
import com.webforj.router.history.Location;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Supplier;

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
  private final BBjSecurityStore store;
  private final BBjThrottleStore throttleStore;
  private final BBjSecurityConfigurationProperties properties;
  private final BBjRouteSecurityConfiguration configuration;
  private final Consumer<Location> navigator;
  private final Clock clock;
  private final Supplier<String> addressSupplier;
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
    throttleStore = builder.throttleStore;
    properties = builder.properties;
    configuration = builder.configuration;
    navigator = builder.navigator;
    clock = builder.clock;
    addressSupplier = builder.addressSupplier;
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
   * An empty or blank user name or password, a user name longer than 256 characters and a password
   * longer than 1024 characters are refused at once as invalid credentials. Otherwise the sign in
   * runs on the calling thread. After a successful sign in the session is saved in the session
   * store. The sign in opens no location.
   * </p>
   *
   * <p>
   * When the session cannot be saved, the sign in fails as unavailable, its login token is ended
   * and the user that was signed in before stays signed in.
   * </p>
   *
   * <p>
   * A user name or a client address that reached its limit of failed sign ins inside the throttle
   * window is refused as throttled without asking BBj. With an address limit of 0, or when the
   * client address cannot be read, only the user name limit applies. Refused credentials count one
   * failed sign in for the user name and for the address. A successful sign in clears the count of
   * the user name and keeps the count of the address. When the counts cannot be read, the sign in
   * fails as unavailable.
   * </p>
   *
   * @param userName the user name
   * @param password the password
   *
   * @return the status of the sign in
   * @throws NullPointerException if any parameter is {@code null}
   */
  BBjLoginStatus login(String userName, String password) {
    Objects.requireNonNull(userName, "userName");
    Objects.requireNonNull(password, "password");
    if (userName.isBlank() || password.isBlank() || userName.length() > MAX_USER_NAME_LENGTH
        || password.length() > MAX_PASSWORD_LENGTH) {
      return BBjLoginStatus.INVALID_CREDENTIALS;
    }

    String address = readAddress().orElse(null);
    try {
      if (isThrottled(userName, address)) {
        logger.log(Level.DEBUG, "A sign in was refused after too many failed attempts");
        return BBjLoginStatus.THROTTLED;
      }
    } catch (IllegalStateException e) {
      logger.log(Level.WARNING, "A sign in was refused because the failed attempts are unknown");
      return BBjLoginStatus.UNAVAILABLE;
    }

    BBjSession opened;
    try {
      opened = gateway.openSession(userName, password);
    } catch (BBjAdminFailure e) {
      if (e.getKind() == Kind.INVALID_CREDENTIALS) {
        addFailure(userName, address);
      }

      return applyFailure(e);
    } catch (RuntimeException e) {
      return applyFailure(createUnavailableFailure());
    }

    BBjLoginStatus status = applySession(opened);
    if (status == BBjLoginStatus.AUTHENTICATED) {
      clearUserAttempts(userName);
    }

    return status;
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

  private Optional<String> readAddress() {
    if (properties.getThrottleAddressMaxAttempts() == 0) {
      return Optional.empty();
    }

    String address;
    try {
      address = addressSupplier.get();
    } catch (RuntimeException e) {
      logger.log(Level.WARNING,
          "The client address could not be read, the address limit is not applied ({0})",
          e.getClass().getName());
      return Optional.empty();
    }

    if (address == null || address.isBlank()) {
      logger.log(Level.WARNING, "The client address is blank, the address limit is not applied");
      return Optional.empty();
    }

    return Optional.of(address);
  }

  private boolean isThrottled(String userName, String address) {
    int userAttempts = throttleStore.readAttempts(BBjThrottleStore.Kind.USER_NAME, userName);
    if (userAttempts >= properties.getThrottleMaxAttempts()) {
      return true;
    }

    return address != null && throttleStore.readAttempts(BBjThrottleStore.Kind.ADDRESS,
        address) >= properties.getThrottleAddressMaxAttempts();
  }

  private void addFailure(String userName, String address) {
    try {
      int userAttempts = throttleStore.addFailure(BBjThrottleStore.Kind.USER_NAME, userName);
      if (userAttempts == properties.getThrottleMaxAttempts()) {
        logger.log(Level.WARNING,
            "A user name reached {0} failed sign ins and is refused for {1} seconds",
            String.valueOf(userAttempts), String.valueOf(properties.getThrottleWindowSeconds()));
      }

      if (address != null) {
        int addressAttempts = throttleStore.addFailure(BBjThrottleStore.Kind.ADDRESS, address);
        if (addressAttempts == properties.getThrottleAddressMaxAttempts()) {
          logger.log(Level.WARNING,
              "The address {0} reached {1} failed sign ins and is refused for {2} seconds", address,
              String.valueOf(addressAttempts),
              String.valueOf(properties.getThrottleWindowSeconds()));
        }
      }
    } catch (IllegalStateException e) {
      logger.log(Level.WARNING, "A failed sign in could not be counted");
    }
  }

  private void clearUserAttempts(String userName) {
    try {
      throttleStore.clearAttempts(BBjThrottleStore.Kind.USER_NAME, userName);
    } catch (IllegalStateException e) {
      logger.log(Level.WARNING, "The failed sign ins of a user name could not be cleared");
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
    private BBjThrottleStore throttleStore;
    private BBjSecurityConfigurationProperties properties;
    private BBjRouteSecurityConfiguration configuration;
    private Consumer<Location> navigator;
    private Supplier<String> addressSupplier;
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
      Objects.requireNonNull(throttleStore, "throttleStore");
      Objects.requireNonNull(properties, "properties");
      Objects.requireNonNull(configuration, "configuration");
      Objects.requireNonNull(navigator, "navigator");
      Objects.requireNonNull(addressSupplier, "addressSupplier");
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

    /**
     * Sets the throttle store of the service.
     *
     * @param throttleStore counts the failed sign in attempts per user name and per address
     *
     * @return this builder
     */
    public Builder setThrottleStore(BBjThrottleStore throttleStore) {
      this.throttleStore = throttleStore;
      return this;
    }

    /**
     * Sets the address supplier of the service.
     *
     * @param addressSupplier gives the address of the client that signs in
     *
     * @return this builder
     */
    public Builder setAddressSupplier(Supplier<String> addressSupplier) {
      this.addressSupplier = addressSupplier;
      return this;
    }
  }
}
