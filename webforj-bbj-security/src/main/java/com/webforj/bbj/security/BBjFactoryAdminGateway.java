package com.webforj.bbj.security;

import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.BBjAdminException;
import com.basis.api.admin.BBjAdminFactory;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.nio.file.Path;
import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;

/**
 * The {@link BBjAdminGateway} that calls the BBj Admin API with the connection settings of a
 * {@link BBjAdminConfiguration}.
 *
 * <p>
 * Every call opens its own connection and releases it before it returns. The gateway keeps no
 * state, so one instance serves all threads.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
final class BBjFactoryAdminGateway implements BBjAdminGateway {
  private static final long CLOSING_MINUTES = 1L;
  private static final Logger logger = System.getLogger(BBjFactoryAdminGateway.class.getName());

  private final Clock clock;
  private final String host;
  private final int port;
  private final boolean ssl;
  private final int timeoutMillis;
  private final String truststore;
  private final String truststorePassword;
  private final Duration tokenLifetime;

  /**
   * Creates a gateway.
   *
   * @param configuration the connection settings
   * @param clock the clock that starts the lifetime of a token
   *
   * @throws NullPointerException if an argument is {@code null}
   */
  BBjFactoryAdminGateway(BBjAdminConfiguration configuration, Clock clock) {
    Objects.requireNonNull(configuration, "configuration");
    this.clock = Objects.requireNonNull(clock, "clock");
    this.host = configuration.getHost();
    this.port = configuration.getPort();
    this.ssl = configuration.isSsl();
    this.timeoutMillis = (int) configuration.getTimeout().toMillis();
    this.truststore = configuration.getTruststore().map(Path::toString).orElse(null);
    this.truststorePassword = truststore == null ? null : configuration.getTruststorePassword();
    this.tokenLifetime = configuration.getTokenLifetime();
  }

  @Override
  public BBjLoginSession openSession(String userName, String password) {
    Objects.requireNonNull(userName, "userName");
    Objects.requireNonNull(password, "password");
    Instant start = clock.instant();
    String token;
    try {
      token =
          BBjAdminFactory.getAuthToken(getAddress(), port, ssl, timeoutMillis, userName, password,
              Locale.ENGLISH, truststore, truststorePassword, tokenLifetime.toMinutes(), Map.of());
    } catch (BBjAdminException | UnknownHostException | RuntimeException e) {
      throw BBjAdminFailureClassifier.toFailure(e, password, truststorePassword);
    }

    try {
      String name = sendCall(token, BBjAdminBase::getUser);

      return new BBjLoginSession(name, token, start.plus(tokenLifetime));
    } catch (BBjAdminException | UnknownHostException | RuntimeException e) {
      BBjAdminFailure failure =
          BBjAdminFailureClassifier.toFailure(e, password, token, truststorePassword);
      closeLeftoverToken(token);

      throw failure;
    }
  }

  @Override
  public String readUserName(BBjLoginSession session) {
    String token = Objects.requireNonNull(session, "session").getToken();
    try {
      return sendCall(token, BBjAdminBase::getUser);
    } catch (BBjAdminException | UnknownHostException | RuntimeException e) {
      throw BBjAdminFailureClassifier.toFailure(e, token, truststorePassword);
    }
  }

  @Override
  public BBjLoginSession updateSession(BBjLoginSession session) {
    String token = Objects.requireNonNull(session, "session").getToken();
    Instant start = clock.instant();
    String newToken = null;
    try {
      newToken = sendCall(token, admin -> admin.renewToken(tokenLifetime.toMinutes(), true));

      return new BBjLoginSession(session.getUserName(), newToken, start.plus(tokenLifetime));
    } catch (BBjAdminException | UnknownHostException | RuntimeException e) {
      throw BBjAdminFailureClassifier.toFailure(e, token, newToken, truststorePassword);
    }
  }

  @Override
  public void closeSession(BBjLoginSession session) {
    String token = Objects.requireNonNull(session, "session").getToken();
    try {
      closeToken(token);
    } catch (BBjAdminException | UnknownHostException | RuntimeException e) {
      throw BBjAdminFailureClassifier.toFailure(e, token, truststorePassword);
    }
  }

  private InetAddress getAddress() throws UnknownHostException {
    return InetAddress.getByName(host);
  }

  private BBjAdminBase openConnection(String token) throws BBjAdminException, UnknownHostException {
    return BBjAdminFactory.getBBjAdmin(getAddress(), port, ssl, timeoutMillis, token, null,
        Locale.ENGLISH, truststore, truststorePassword);
  }

  private <T> T sendCall(String token, AdminCall<T> call)
      throws BBjAdminException, UnknownHostException {
    BBjAdminBase admin = openConnection(token);
    try {
      return call.apply(admin);
    } finally {
      admin.release();
    }
  }

  private void closeToken(String token) throws BBjAdminException, UnknownHostException {
    sendCall(token, admin -> admin.renewToken(CLOSING_MINUTES, true));
  }

  private void closeLeftoverToken(String token) {
    try {
      closeToken(token);
    } catch (BBjAdminException | UnknownHostException | RuntimeException e) {
      logger.log(Level.WARNING, "A BBj login token could not be ended and expires on its own");
    }
  }

  @FunctionalInterface
  private interface AdminCall<T> {
    T apply(BBjAdminBase admin) throws BBjAdminException;
  }
}
