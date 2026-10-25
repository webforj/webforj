package com.webforj.bbj.security;

import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Consumer;

import com.webforj.PendingResult;
import com.webforj.bbj.security.model.BBjPrincipal;
import com.webforj.bbj.security.model.BBjSession;
import com.webforj.bbj.security.model.BBjLoginStatus;
import com.webforj.router.history.Location;

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
  private final BBjSecurityConfigurationProperties properties;
  private final Consumer<Location> navigator;
  private BBjSession session;

  /**
   * Creates a service.
   *
   * @param gateway opens the sessions
   * @param worker runs the sign in away from the calling thread
   * @param locationStore holds the location the user requested before signing in
   * @param properties the configuration of the sign in
   * @param navigator opens the location the user lands on after signing in
   *
   * @throws NullPointerException if any parameter is {@code null}
   */
  BBjAuthenticationService(BBjAdminGateway gateway, BBjAdminWorker worker,
      BBjRequestedLocationStore locationStore, BBjSecurityConfigurationProperties properties,
      Consumer<Location> navigator) {
    this.gateway = Objects.requireNonNull(gateway, "gateway");
    this.worker = Objects.requireNonNull(worker, "worker");
    this.locationStore = Objects.requireNonNull(locationStore, "locationStore");
    this.properties = Objects.requireNonNull(properties, "properties");
    this.navigator = Objects.requireNonNull(navigator, "navigator");
  }

  /**
   * Starts signing a user in.
   *
   * <p>
   * An empty or blank user name or password, a user name longer than 256 characters and a password
   * longer than 1024 characters are refused at once as invalid credentials. Otherwise the sign in
   * runs in the background. After a successful sign in the service opens the location the user
   * requested before signing in, or the default success path when no location was requested or
   * returning is switched off. The requested location is cleared either way.
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

    PendingResult<BBjLoginStatus> result =
        worker.start(() -> gateway.openSession(userName, password), this::applySession,
            this::applyFailure, this::discardLateSession);
    result.thenAccept(status -> {
      if (status == BBjLoginStatus.AUTHENTICATED) {
        openTargetLocation();
      }
    });

    return result;
  }

  /**
   * Gets the user that signed in.
   *
   * @return the signed in user, or empty if nobody has signed in
   */
  Optional<BBjPrincipal> currentUser() {
    return Optional.ofNullable(session).map(BBjSession::getPrincipal);
  }

  private BBjLoginStatus applySession(BBjSession session) {
    this.session = session;

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
        logger.log(Level.WARNING, "The BBj Admin API could not complete the sign in");

        yield BBjLoginStatus.UNAVAILABLE;
      }
    };
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

  private void discardLateSession(BBjSession session) {}
}
