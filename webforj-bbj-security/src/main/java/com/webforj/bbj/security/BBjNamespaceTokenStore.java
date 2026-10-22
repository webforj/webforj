package com.webforj.bbj.security;

import com.webforj.environment.namespace.PrivateNamespace;
import com.webforj.environment.namespace.exception.NamespaceLockedException;
import com.webforj.webstorage.SessionStorage;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Clock;
import java.time.Instant;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HexFormat;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Supplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Keeps the login session encrypted in a private namespace of BBjServices.
 *
 * <p>
 * The namespace holds one entry per signed in tab, named by the format version, the expiry and a
 * random handle. The tab's session storage holds the entry name and the key that opens the entry,
 * so the namespace never sees the key and the browser never sees the token. A new key and a new
 * handle are made on every save.
 * </p>
 *
 * <p>
 * Every save removes expired entries and, at the limit of 10000 entries, the entries that expire
 * first. Names that do not have the format of this class are never read or removed. Nothing secret
 * is logged. An exception of the storage or the namespace is logged by its class name only.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
final class BBjNamespaceTokenStore implements BBjTokenStore {
  private static final Logger logger = System.getLogger(BBjNamespaceTokenStore.class.getName());
  private static final String HANDLE_KEY = "webforj-bbj-handle";
  private static final String KEY_KEY = "webforj-bbj-key";
  private static final String NAME_PREFIX = "bbj-security-tokens-";
  private static final String VERSION = "v1.";
  private static final String NOT_KEPT = "The sign-in could not be kept";
  private static final String FAILURE = "{0} ({1})";
  private static final Pattern ENTRY_NAME = Pattern.compile("v1\\.(\\d{1,15})\\.[A-Za-z0-9_-]{43}");
  private static final Pattern UNSAFE_CHARACTERS = Pattern.compile("[^A-Za-z0-9_-]");
  private static final int MAX_APPLICATION_NAME_LENGTH = 64;
  private static final int MAX_ENTRIES = 10000;

  private final Supplier<PrivateNamespace> namespaceSupplier;
  private final Supplier<SessionStorage> storageSupplier;
  private final Clock clock;
  private final BBjTokenEntryCipher cipher = new BBjTokenEntryCipher();
  private PrivateNamespace namespace;

  BBjNamespaceTokenStore(Supplier<PrivateNamespace> namespaceSupplier,
      Supplier<SessionStorage> storageSupplier, Clock clock) {
    this.namespaceSupplier = namespaceSupplier;
    this.storageSupplier = storageSupplier;
    this.clock = clock;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void saveSession(BBjLoginSession session) {
    SessionStorage storage = storageSupplier.get();
    if (storage == null) {
      throw new IllegalStateException(NOT_KEPT);
    }

    PrivateNamespace target;
    try {
      target = openNamespace();
    } catch (RuntimeException e) {
      logger.log(Level.WARNING, FAILURE, "The namespace could not be opened",
          e.getClass().getName());
      throw new IllegalStateException(NOT_KEPT);
    }

    Map<String, String> previous = storage.get(List.of(HANDLE_KEY, KEY_KEY));
    String key = cipher.createKey();
    String name = VERSION + session.getExpiresAt().toEpochMilli() + "." + cipher.createHandle();
    String entry = cipher.formatEntry(name, key, session);
    removeStaleEntries(target);

    try {
      target.put(name, entry);
    } catch (NamespaceLockedException | RuntimeException e) {
      logger.log(Level.WARNING, FAILURE, "The sign-in entry could not be written",
          e.getClass().getName());
      throw new IllegalStateException(NOT_KEPT);
    }

    try {
      storage.add(Map.of(HANDLE_KEY, name, KEY_KEY, key));
    } catch (RuntimeException e) {
      logger.log(Level.WARNING, FAILURE, "The sign-in could not be written to the tab",
          e.getClass().getName());
      removeEntries(target, List.of(name));
      resetPair(storage, previous);
      throw new IllegalStateException(NOT_KEPT);
    }

    String previousName = previous.get(HANDLE_KEY);
    if (previousName != null && !previousName.equals(name)
        && removeEntries(target, List.of(previousName)) == 0) {
      logger.log(Level.WARNING, "The previous sign-in entry could not be removed");
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Optional<BBjLoginSession> readSession() {
    try {
      SessionStorage storage = storageSupplier.get();
      if (storage == null) {
        return Optional.empty();
      }

      Map<String, String> pair = storage.get(List.of(HANDLE_KEY, KEY_KEY));
      if (pair.isEmpty()) {
        return Optional.empty();
      }

      String name = pair.get(HANDLE_KEY);
      String key = pair.get(KEY_KEY);
      Optional<Instant> expiresAt = findExpiry(name);
      if (key == null || expiresAt.isEmpty() || !expiresAt.get().isAfter(clock.instant())) {
        logger.log(Level.DEBUG, "A stored sign-in is not usable and was removed");
        clearSession();
        return Optional.empty();
      }

      Object value = openNamespace().getOrDefault(name, null);
      if (!(value instanceof String)) {
        logger.log(Level.DEBUG, "A stored sign-in has no entry and was removed");
        clearSession();
        return Optional.empty();
      }

      Optional<BBjLoginSession> session = cipher.parseEntry(name, key, (String) value);
      if (session.isEmpty()) {
        logger.log(Level.WARNING, "A stored sign-in could not be read and was removed");
        clearSession();
      }

      return session;
    } catch (RuntimeException e) {
      logger.log(Level.WARNING, FAILURE, "The stored sign-in could not be read",
          e.getClass().getName());
      return Optional.empty();
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void clearSession() {
    SessionStorage storage;
    try {
      storage = storageSupplier.get();
    } catch (RuntimeException e) {
      logger.log(Level.WARNING, FAILURE, "The tab could not be read", e.getClass().getName());
      return;
    }

    if (storage == null) {
      return;
    }

    try {
      String name = storage.get(List.of(HANDLE_KEY, KEY_KEY)).get(HANDLE_KEY);
      if (name != null && removeEntries(openNamespace(), List.of(name)) == 0) {
        logger.log(Level.WARNING, "The sign-in entry could not be removed");
      }
    } catch (RuntimeException e) {
      logger.log(Level.WARNING, FAILURE, "The sign-in entry could not be removed",
          e.getClass().getName());
    }

    try {
      storage.remove(HANDLE_KEY, KEY_KEY);
    } catch (RuntimeException e) {
      logger.log(Level.WARNING, FAILURE, "The sign-in could not be removed from the tab",
          e.getClass().getName());
    }
  }

  /**
   * Creates the name of the namespace of one app.
   *
   * <p>
   * A name that holds a character other than a letter, a digit, an underscore or a hyphen, or that
   * is empty or longer than 64 characters, is replaced by a safe form followed by a short hash of
   * the original name, so that two apps never share a namespace.
   * </p>
   *
   * @param applicationName the registered application name
   * @return the name below the prefix of the namespace
   */
  static String createNamespaceName(String applicationName) {
    String safe = UNSAFE_CHARACTERS.matcher(applicationName).replaceAll("_");
    if (safe.equals(applicationName) && !safe.isEmpty()
        && safe.length() <= MAX_APPLICATION_NAME_LENGTH) {
      return NAME_PREFIX + safe;
    }

    String base = safe.isEmpty() ? "app"
        : safe.substring(0, Math.min(safe.length(), MAX_APPLICATION_NAME_LENGTH));

    return NAME_PREFIX + base + "-" + createHash(applicationName);
  }

  private PrivateNamespace openNamespace() {
    if (namespace == null) {
      namespace = namespaceSupplier.get();
    }

    return namespace;
  }

  private Optional<Instant> findExpiry(String name) {
    if (name == null) {
      return Optional.empty();
    }

    Matcher matcher = ENTRY_NAME.matcher(name);
    if (!matcher.matches()) {
      return Optional.empty();
    }

    return Optional.of(Instant.ofEpochMilli(Long.parseLong(matcher.group(1))));
  }

  private void removeStaleEntries(PrivateNamespace target) {
    try {
      Map<String, Instant> entries = new HashMap<>();
      for (String name : target.keySet()) {
        findExpiry(name).ifPresent(expiresAt -> entries.put(name, expiresAt));
      }

      Instant now = clock.instant();
      List<String> expired = new ArrayList<>();
      List<String> live = new ArrayList<>();
      entries.forEach((name, expiresAt) -> (expiresAt.isAfter(now) ? live : expired).add(name));
      removeEntries(target, expired);
      live.sort((first, second) -> entries.get(first).compareTo(entries.get(second)));

      int surplus = live.size() - MAX_ENTRIES + 1;
      if (surplus > 0) {
        int removed = removeEntries(target, live.subList(0, surplus));
        logger.log(Level.WARNING,
            "The token store removed {0} sign-ins because its limit of {1} was reached", removed,
            String.valueOf(MAX_ENTRIES));
      }
    } catch (RuntimeException e) {
      logger.log(Level.WARNING, FAILURE, "Expired sign-ins could not be removed",
          e.getClass().getName());
    }
  }

  private int removeEntries(PrivateNamespace target, List<String> names) {
    int removed = 0;
    for (String name : names) {
      try {
        target.remove(name);
        removed++;
      } catch (NamespaceLockedException | RuntimeException e) {
        logger.log(Level.DEBUG, FAILURE, "A sign-in entry could not be removed",
            e.getClass().getName());
      }
    }

    return removed;
  }

  private void resetPair(SessionStorage storage, Map<String, String> previous) {
    try {
      if (previous.size() == 2) {
        storage.add(previous);
        return;
      }
    } catch (RuntimeException e) {
      logger.log(Level.WARNING, FAILURE, "The previous sign-in could not be restored",
          e.getClass().getName());
    }

    storage.remove(HANDLE_KEY, KEY_KEY);
  }

  private static String createHash(String text) {
    try {
      byte[] digest =
          MessageDigest.getInstance("SHA-256").digest(text.getBytes(StandardCharsets.UTF_8));
      return HexFormat.of().formatHex(digest, 0, 4);
    } catch (NoSuchAlgorithmException e) {
      throw new IllegalStateException("The JVM has no SHA-256", e);
    }
  }
}
