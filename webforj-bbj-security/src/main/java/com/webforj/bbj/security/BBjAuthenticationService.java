package com.webforj.bbj.security;

import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Consumer;

import com.webforj.PendingResult;
import com.webforj.bbj.security.model.BBjPrincipal;
import com.webforj.router.history.Location;

/**
 * BBjAuthenticationService
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

  BBjAuthenticationService(BBjAdminGateway gateway, BBjAdminWorker worker,
      BBjRequestedLocationStore locationStore, BBjSecurityConfigurationProperties properties,
      Consumer<Location> navigator) {
    this.gateway = Objects.requireNonNull(gateway, "gateway");
    this.worker = Objects.requireNonNull(worker, "worker");
    this.locationStore = Objects.requireNonNull(locationStore, "locationStore");
    this.properties = Objects.requireNonNull(properties, "properties");
    this.navigator = Objects.requireNonNull(navigator, "navigator");
  }

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
