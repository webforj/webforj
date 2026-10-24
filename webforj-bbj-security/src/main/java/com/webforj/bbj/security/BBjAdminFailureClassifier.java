package com.webforj.bbj.security;

import com.basis.api.admin.BBjAdminException;
import com.webforj.bbj.security.BBjAdminFailure.Kind;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Objects;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * Turns an exception of an Admin API call into a {@link BBjAdminFailure}.
 *
 * <p>
 * The kind comes from the message of the exception and of its causes. Only a
 * {@link BBjAdminException} carries a verdict of the server. Every other exception and every text
 * that is not known is {@link Kind#UNAVAILABLE}.
 * </p>
 *
 * <p>
 * The message of the failure contains the class names and messages of the causes. Secrets handed in
 * and anything shaped like a token are replaced, control characters are removed and the text is cut
 * at 300 characters.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
final class BBjAdminFailureClassifier {
  private static final String MASK = "***";
  private static final String REFUSED_LOGIN_PREFIX = "server failed login:";
  private static final String ENTRY_SEPARATOR = " / ";
  private static final int MAX_CAUSES = 5;
  private static final int MAX_DETAIL_LENGTH = 300;
  private static final char FIRST_PRINTABLE = ' ';
  private static final char DELETE = '\u007f';
  private static final Pattern TOKEN_SHAPE =
      Pattern.compile("eyJ[A-Za-z0-9_-]*\\.[A-Za-z0-9_-]+\\.[A-Za-z0-9_-]+");

  private BBjAdminFailureClassifier() {}

  /**
   * Creates the failure for an exception.
   *
   * @param cause the exception of the call
   * @param secrets the texts to hide, empty and {@code null} entries are skipped
   * @return the failure
   *
   * @throws NullPointerException if the cause is {@code null}
   */
  static BBjAdminFailure toFailure(Throwable cause, String... secrets) {
    List<Throwable> causes = readCauses(Objects.requireNonNull(cause, "cause"));
    Kind kind = cause instanceof BBjAdminException ? findKind(causes) : Kind.UNAVAILABLE;

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

  private static Kind findKind(List<Throwable> causes) {
    List<String> messages = causes.stream().map(Throwable::getMessage).filter(Objects::nonNull)
        .map(message -> message.toLowerCase(Locale.ROOT)).toList();
    boolean refusedLogin = messages.stream()
        .anyMatch(message -> message.stripLeading().startsWith(REFUSED_LOGIN_PREFIX));
    if (refusedLogin) {
      return Kind.TOKEN_REJECTED;
    }

    String text = String.join("\n", messages);
    for (KnownMessage known : KnownMessage.values()) {
      if (text.contains(known.text)) {
        return known.kind;
      }
    }

    return Kind.UNAVAILABLE;
  }

  private static String getPrefix(Kind kind) {
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

  private static String hideSecrets(String text, String[] secrets) {
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

  private enum KnownMessage {
    INVALID_PASSWORD("invalid username/password", Kind.INVALID_CREDENTIALS), INVALID_CODE(
        "invalid 2-factor authentication code", Kind.INVALID_CREDENTIALS), CODE_REQUIRED(
            "2-factor authentication code required", Kind.TWO_FACTOR_REQUIRED), TOKEN_EXPIRED(
                "authentication token is expired", Kind.TOKEN_REJECTED), TOKEN_UNVERIFIED(
                    "unable to verify authentication token", Kind.TOKEN_REJECTED);

    private final String text;
    private final Kind kind;

    KnownMessage(String text, Kind kind) {
      this.text = text;
      this.kind = kind;
    }
  }
}
