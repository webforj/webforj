package com.webforj.bbj.security;

import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.BBjAdminException;
import com.basis.api.admin.BBjAdminFactory;
import com.webforj.bbj.security.model.BBjPermissions;
import com.webforj.bbj.security.model.BBjPrincipal;
import com.webforj.bbj.security.model.BBjSession;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.regex.Pattern;

/**
 * Opens sessions for users through the BBj Admin API.
 *
 * <p>
 * The host, the port, the use of SSL, the truststore, the time limit and the lifetime of the token
 * come from the {@link BBjSecurityConfigurationProperties} that were given when the gateway was
 * created. A failure never carries the password, the truststore password or the token in its
 * message.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
public final class BBjAdminGateway {
  private static final Logger logger = System.getLogger(BBjAdminGateway.class.getName());
  private static final String CLOSED_TOKEN_NOTE =
      "A BBj login token could not be read after it was issued and expires on its own";
  private static final String MASK = "***";
  private static final String REFUSED_LOGIN_PREFIX = "server failed login:";
  private static final String ENTRY_SEPARATOR = " / ";
  private static final int MAX_CAUSES = 5;
  private static final int MAX_DETAIL_LENGTH = 300;
  private static final char FIRST_PRINTABLE = ' ';
  private static final char DELETE = '\u007f';
  private static final Pattern TOKEN_SHAPE =
      Pattern.compile("eyJ[A-Za-z0-9_-]*\\.[A-Za-z0-9_-]+\\.[A-Za-z0-9_-]+");
  private final String host;
  private final int port;
  private final boolean ssl;
  private final String truststore;
  private final String truststorePassword;
  private final int timeoutMillis;
  private final Duration tokenLifetime;
  private final Clock clock;

  /**
   * Creates a gateway that measures token lifetimes with the system clock.
   *
   * @param properties the configuration of the Admin API connection
   *
   * @throws NullPointerException if {@code properties} is {@code null}
   */
  BBjAdminGateway(BBjSecurityConfigurationProperties properties) {
    this(properties, Clock.systemUTC());
  }

  /**
   * Creates a gateway that measures token lifetimes with the given clock.
   *
   * @param properties the configuration of the Admin API connection
   * @param clock the clock that gives the start of a session
   *
   * @throws NullPointerException if any parameter is {@code null}
   */
  BBjAdminGateway(BBjSecurityConfigurationProperties properties, Clock clock) {
    Objects.requireNonNull(properties, "properties");
    this.clock = Objects.requireNonNull(clock, "clock");
    host = properties.getAdminHost();
    port = properties.getAdminPort();
    ssl = properties.getAdminSsl();
    truststore = properties.getAdminTruststore();
    truststorePassword = properties.getAdminTruststorePassword();
    timeoutMillis = (int) Duration.ofSeconds(properties.getAdminTimeoutSeconds()).toMillis();
    tokenLifetime = Duration.ofMinutes(properties.getTokenMinutes());
  }

  /**
   * Signs a user in and opens a session for them.
   *
   * <p>
   * The call blocks until the Admin API answers or the time limit passes. The principal of the
   * session carries the user name the Admin API reports, the names of the BBj security groups of
   * the user as roles, and the names of the {@link BBjPermissions} the user holds as authorities.
   * The session expires after the configured token lifetime, counted from the start of the call.
   * When the user name, the group names or the permissions cannot be read after the token was
   * issued, the token is not revoked and expires on its own.
   * </p>
   *
   * @param userName the user name
   * @param password the password
   * @param code the 2 factor code, or {@code null} to sign in with the password only
   *
   * @return the opened session
   * @throws NullPointerException if {@code userName} or {@code password} is {@code null}
   * @throws BBjAdminFailure with the kind {@code INVALID_CREDENTIALS} when the user name or the
   *         password is refused, {@code INVALID_CODE} when the 2 factor code is refused,
   *         {@code TWO_FACTOR_REQUIRED} when the account needs a 2 factor code,
   *         {@code TOKEN_REJECTED} when the server refuses the issued token, and
   *         {@code UNAVAILABLE} when the Admin API cannot be reached or fails otherwise
   */
  BBjSession openSession(String userName, String password, String code) {
    Objects.requireNonNull(userName, "userName");
    Objects.requireNonNull(password, "password");
    Instant start = clock.instant();
    String token =
        code == null ? fetchToken(userName, password) : fetchToken(userName, password, code);

    try {
      var principal = sendCall(token, admin -> readPrincipal(admin, token));

      return new BBjSession(principal, token, start.plus(tokenLifetime));
    } catch (BBjAdminException | UnknownHostException | RuntimeException e) {
      BBjAdminFailure failure = toFailure(CallKind.TOKEN, e, password, code, token);
      logger.log(Level.WARNING, CLOSED_TOKEN_NOTE);

      throw failure;
    }
  }

  /**
   * Ends the login token of a session.
   *
   * <p>
   * The call blocks until the Admin API answers or the time limit passes. Nothing happens when the
   * session is {@code null}.
   * </p>
   *
   * @param session the session whose token is ended
   *
   * @throws BBjAdminFailure with the kind {@code TOKEN_REJECTED} when the server refuses the token,
   *         for example because it ended already, and {@code UNAVAILABLE} when the Admin API cannot
   *         be reached or fails otherwise
   */
  void closeSession(BBjSession session) {
    if (session == null) {
      return;
    }
    var token = session.getToken();
    try {
      sendCall(token, admin -> {
        admin.invalidateToken(token);
        return null;
      });
    } catch (BBjAdminException | UnknownHostException | RuntimeException e) {
      throw toFailure(CallKind.TOKEN, e, token);
    }
  }

  String readUserName(BBjSession session) {
    if (session == null) {
      return null;
    }
    var token = session.getToken();
    try {
      return sendCall(token, BBjAdminBase::getUser);
    } catch (BBjAdminException | UnknownHostException | RuntimeException e) {
      throw toFailure(CallKind.TOKEN, e, token);
    }
  }

  BBjSession renewSession(BBjSession session) {
    if (session == null) {
      return null;
    }
    var oldToken = session.getToken();
    String newToken = null;
    try {
      var minutes = tokenLifetime.getSeconds() / 60;
      newToken = sendCall(oldToken, admin -> admin.renewToken(minutes, true));
      var expiry = clock.instant().plus(tokenLifetime);
      var token = newToken;
      var principal = sendCall(newToken, admin -> readPrincipal(admin, token));

      return new BBjSession(principal, newToken, expiry);
    } catch (Exception e) {
      throw toFailure(CallKind.TOKEN, e, oldToken, newToken);
    }
  }

  private InetAddress getAddress() throws UnknownHostException {
    return InetAddress.getByName(host);
  }

  private String fetchToken(String userName, String password) {
    try {
      return BBjAdminFactory.getAuthToken(getAddress(), port, ssl, timeoutMillis, userName,
          password, Locale.ENGLISH, truststore, truststorePassword, tokenLifetime.toMinutes(),
          Map.of());
    } catch (BBjAdminException | UnknownHostException | RuntimeException e) {
      throw toFailure(CallKind.SIGN_IN, e, password);
    }
  }

  private String fetchToken(String userName, String password, String code) {
    BBjAdminBase admin;
    try {
      admin = BBjAdminFactory.getBBjAdmin(getAddress(), port, ssl, timeoutMillis, userName,
          password, Locale.ENGLISH, truststore, truststorePassword, code);
    } catch (BBjAdminException | UnknownHostException | RuntimeException e) {
      throw toFailure(CallKind.SIGN_IN, e, password, code);
    }

    String codeToken = null;
    try {
      codeToken = admin.getTokenValue();
      return admin.renewToken(codeToken, tokenLifetime.toMinutes());
    } catch (BBjAdminException | RuntimeException e) {
      throw toFailure(CallKind.SIGN_IN, e, password, code, codeToken);
    } finally {
      admin.release();
    }
  }

  private <T> T sendCall(String token, AdminCall<? extends T> call)
      throws BBjAdminException, UnknownHostException {
    BBjAdminBase admin = BBjAdminFactory.getBBjAdmin(getAddress(), port, ssl, timeoutMillis, token,
        null, Locale.ENGLISH, truststore, truststorePassword);
    try {
      return call.apply(admin);
    } finally {
      admin.release();
    }
  }

  private BBjPrincipal readPrincipal(BBjAdminBase admin, String token) throws BBjAdminException {
    var name = admin.getUser();
    var groups = admin.getSecurityGroupNames();

    var permissions = admin.getSingleUserPermissions(token);
    Set<String> authorities = new HashSet<>();
    for (BBjPermissions permission : BBjPermissions.values()) {
      if (permissions.getBoolean(permission.toString())) {
        authorities.add(permission.toString());
      }
    }

    return new BBjPrincipal(name, Set.copyOf(groups), Set.copyOf(authorities));
  }

  private BBjAdminFailure toFailure(CallKind callKind, Throwable cause, String... secrets) {
    List<Throwable> causes = readCauses(Objects.requireNonNull(cause, "cause"));
    BBjAdminFailure.Kind kind = cause instanceof BBjAdminException ? findKind(callKind, causes)
        : BBjAdminFailure.Kind.UNAVAILABLE;
    String[] hidden = Arrays.copyOf(secrets, secrets.length + 1);
    hidden[secrets.length] = truststorePassword;

    return new BBjAdminFailure(kind, getPrefix(kind) + ": " + createDetail(causes, hidden));
  }

  private static List<Throwable> readCauses(Throwable cause) {
    Set<Throwable> seen = Collections.newSetFromMap(new IdentityHashMap<>());
    List<Throwable> causes = new ArrayList<>();
    Throwable current = cause;
    while (current != null && causes.size() < MAX_CAUSES && seen.add(current)) {
      causes.add(current);
      current = current.getCause();
    }

    return causes;
  }

  private static BBjAdminFailure.Kind findKind(CallKind callKind, List<Throwable> causes) {
    List<String> messages = causes.stream().map(Throwable::getMessage).filter(Objects::nonNull)
        .map(message -> message.toLowerCase(Locale.ROOT)).toList();
    boolean refusedLogin = messages.stream()
        .anyMatch(message -> message.stripLeading().startsWith(REFUSED_LOGIN_PREFIX));
    if (refusedLogin && callKind == CallKind.TOKEN) {
      return BBjAdminFailure.Kind.TOKEN_REJECTED;
    }

    String text = String.join("\n", messages);
    for (KnownMessage known : KnownMessage.values()) {
      if (text.contains(known.text)) {
        return known.kind;
      }
    }

    return refusedLogin ? BBjAdminFailure.Kind.TOKEN_REJECTED : BBjAdminFailure.Kind.UNAVAILABLE;
  }

  private static String getPrefix(BBjAdminFailure.Kind kind) {
    return switch (kind) {
      case INVALID_CREDENTIALS -> "BBj Admin API refused the credentials";
      case INVALID_CODE -> "BBj Admin API refused the 2-factor code";
      case TWO_FACTOR_REQUIRED -> "BBj Admin API requires a 2-factor code";
      case TOKEN_REJECTED -> "BBj Admin API refused the token";
      case UNAVAILABLE -> "BBj Admin API is unavailable";
    };
  }

  private static String createDetail(List<Throwable> causes, String[] secrets) {
    List<String> entries = new ArrayList<>();
    for (Throwable cause : causes) {
      String entry = formatEntry(cause);
      if (entries.isEmpty() || !entries.getLast().equals(entry)) {
        entries.add(entry);
      }
    }

    String detail = hideSecrets(String.join(ENTRY_SEPARATOR, entries), secrets);
    StringBuilder clean = new StringBuilder(detail.length());
    detail.chars().forEach(character -> clean
        .append(character < FIRST_PRINTABLE || character == DELETE ? ' ' : (char) character));

    return clean.length() > MAX_DETAIL_LENGTH ? clean.substring(0, MAX_DETAIL_LENGTH)
        : clean.toString();
  }

  private static String formatEntry(Throwable cause) {
    String name = cause.getClass().getSimpleName();
    if (name.isEmpty()) {
      name = cause.getClass().getName();
    }

    String message = cause.getMessage();

    return message == null || message.isBlank() ? name : name + ": " + message;
  }

  private static String hideSecrets(String text, String... secrets) {
    String hidden = text;
    if (secrets != null) {
      List<String> ordered =
          Arrays.stream(secrets).filter(secret -> secret != null && !secret.isEmpty()).distinct()
              .sorted(Comparator.comparingInt(String::length).reversed()).toList();
      for (String secret : ordered) {
        hidden = hidden.replace(secret, MASK);
      }
    }

    return TOKEN_SHAPE.matcher(hidden).replaceAll(MASK);
  }

  /**
   * A call on an Admin API connection that returns a value.
   *
   * @param <T> the type of the value
   */
  @FunctionalInterface
  private interface AdminCall<T> {
    T apply(BBjAdminBase admin) throws BBjAdminException;
  }

  private enum CallKind {
    SIGN_IN, TOKEN
  }

  private enum KnownMessage {
    INVALID_PASSWORD("invalid username/password",
        BBjAdminFailure.Kind.INVALID_CREDENTIALS), INVALID_CODE(
            "invalid 2-factor authentication code",
            BBjAdminFailure.Kind.INVALID_CODE), CODE_REQUIRED(
                "2-factor authentication code required",
                BBjAdminFailure.Kind.TWO_FACTOR_REQUIRED), TOKEN_EXPIRED(
                    "authentication token is expired",
                    BBjAdminFailure.Kind.TOKEN_REJECTED), TOKEN_UNVERIFIED(
                        "unable to verify authentication token",
                        BBjAdminFailure.Kind.TOKEN_REJECTED);

    private final String text;
    private final BBjAdminFailure.Kind kind;

    KnownMessage(String text, BBjAdminFailure.Kind kind) {
      this.text = text;
      this.kind = kind;
    }
  }
}
