package com.webforj.bbj.security;

import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.BBjAdminException;
import com.basis.api.admin.BBjAdminFactory;
import com.webforj.bbj.security.model.BBjPrincipal;
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
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.regex.Pattern;

/**
 * BBjAdminGateway
 */
public final class BBjAdminGateway {
  private static final Logger logger = System.getLogger(BBjAdminGateway.class.getName());
  private static final String CLOSED_TOKEN_NOTE = "A BBj login token could not be read after it was issued and expires on its own";
  private static final String MASK = "***";
  private static final String REFUSED_LOGIN_PREFIX = "server failed login:";
  private static final String ENTRY_SEPARATOR = " / ";
  private static final int MAX_CAUSES = 5;
  private static final int MAX_DETAIL_LENGTH = 300;
  private static final char FIRST_PRINTABLE = ' ';
  private static final char DELETE = '\u007f';
  private static final Pattern TOKEN_SHAPE = Pattern.compile("eyJ[A-Za-z0-9_-]*\\.[A-Za-z0-9_-]+\\.[A-Za-z0-9_-]+");
  private final String host;
  private final int port;
  private final int timeoutMillis;
  private final Duration tokenLifetime;
  private final Clock clock;

  BBjAdminGateway(BBjSecurityConfigurationProperties properties) {
    this(properties, Clock.systemUTC());
  }

  BBjAdminGateway(BBjSecurityConfigurationProperties properties, Clock clock) {
    Objects.requireNonNull(properties, "properties");
    this.clock = Objects.requireNonNull(clock, "clock");
    host = properties.getAdminHost();
    port = properties.getAdminPort();
    timeoutMillis = (int) Duration.ofSeconds(properties.getAdminTimeoutSeconds()).toMillis();
    tokenLifetime = Duration.ofMinutes(properties.getTokenMinutes());
  }

  BBjSession openSession(String userName, String password) {
    Objects.requireNonNull(userName, "userName");
    Objects.requireNonNull(password, "password");
    Instant start = clock.instant();
    String token;
    try {
      token = BBjAdminFactory.getAuthToken(getAddress(), port, false, timeoutMillis, userName,
          password, Locale.ENGLISH, null, null, tokenLifetime.toMinutes(), Map.of());
    } catch (BBjAdminException | UnknownHostException | RuntimeException e) {
      throw toFailure(e, password);
    }

    try {
      String name = sendCall(token, BBjAdminBase::getUser);
      BBjPrincipal principal = new BBjPrincipal(name, Set.of(), Set.of());

      return new BBjSession(principal, token, start.plus(tokenLifetime));
    } catch (BBjAdminException | UnknownHostException | RuntimeException e) {
      BBjAdminFailure failure = toFailure(e, password, token);
      logger.log(Level.WARNING, CLOSED_TOKEN_NOTE);

      throw failure;
    }
  }

  private InetAddress getAddress() throws UnknownHostException {
    return InetAddress.getByName(host);
  }

  private <T> T sendCall(String token, AdminCall<? extends T> call)
      throws BBjAdminException, UnknownHostException {
    BBjAdminBase admin = BBjAdminFactory.getBBjAdmin(getAddress(), port, false, timeoutMillis,
        token, null, Locale.ENGLISH, null, null);
    try {
      return call.apply(admin);
    } finally {
      admin.release();
    }
  }

  private static BBjAdminFailure toFailure(Throwable cause, String... secrets) {
    List<Throwable> causes = readCauses(Objects.requireNonNull(cause, "cause"));
    BBjAdminFailure.Kind kind = cause instanceof BBjAdminException ? findKind(causes)
        : BBjAdminFailure.Kind.UNAVAILABLE;

    return new BBjAdminFailure(kind, getPrefix(kind) + ": " + createDetail(causes, secrets));
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

  private static BBjAdminFailure.Kind findKind(List<Throwable> causes) {
    List<String> messages = causes.stream().map(Throwable::getMessage).filter(Objects::nonNull)
        .map(message -> message.toLowerCase(Locale.ROOT)).toList();
    boolean refusedLogin = messages.stream()
        .anyMatch(message -> message.stripLeading().startsWith(REFUSED_LOGIN_PREFIX));
    if (refusedLogin) {
      return BBjAdminFailure.Kind.TOKEN_REJECTED;
    }

    String text = String.join("\n", messages);
    for (KnownMessage known : KnownMessage.values()) {
      if (text.contains(known.text)) {
        return known.kind;
      }
    }

    return BBjAdminFailure.Kind.UNAVAILABLE;
  }

  private static String getPrefix(BBjAdminFailure.Kind kind) {
    return switch (kind) {
      case INVALID_CREDENTIALS -> "BBj Admin API refused the credentials";
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
      List<String> ordered = Arrays.stream(secrets).filter(secret -> secret != null && !secret.isEmpty()).distinct()
          .sorted(Comparator.comparingInt(String::length).reversed()).toList();
      for (String secret : ordered) {
        hidden = hidden.replace(secret, MASK);
      }
    }

    return TOKEN_SHAPE.matcher(hidden).replaceAll(MASK);
  }

  /**
   * AdminCall
   */
  @FunctionalInterface
  private interface AdminCall<T> {
    T apply(BBjAdminBase admin) throws BBjAdminException;
  }

  private enum KnownMessage {
    INVALID_PASSWORD("invalid username/password", BBjAdminFailure.Kind.INVALID_CREDENTIALS),
    INVALID_CODE("invalid 2-factor authentication code",
        BBjAdminFailure.Kind.INVALID_CREDENTIALS),
    CODE_REQUIRED("2-factor authentication code required",
        BBjAdminFailure.Kind.TWO_FACTOR_REQUIRED),
    TOKEN_EXPIRED("authentication token is expired", BBjAdminFailure.Kind.TOKEN_REJECTED),
    TOKEN_UNVERIFIED("unable to verify authentication token",
        BBjAdminFailure.Kind.TOKEN_REJECTED);

    private final String text;
    private final BBjAdminFailure.Kind kind;

    KnownMessage(String text, BBjAdminFailure.Kind kind) {
      this.text = text;
      this.kind = kind;
    }
  }
}
