package com.webforj.bbj.security;

import com.webforj.environment.namespace.GlobalNamespace;
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
 * Keeps the login session encrypted in the global namespace of BBjServices.
 *
 * <p>
 * The namespace holds one entry per signed in tab. The key of an entry is the prefix of the
 * application, the format version, the expiry and a random handle. The tab's session storage holds
 * the key of the entry and the key that opens the entry, so the namespace never sees the opening
 * key and the browser never sees the token. A new key and a new handle are made on every save.
 * </p>
 *
 * <p>
 * Every save removes expired entries of the application and, at the limit of 10000 entries of the
 * application, the entries that expire first. Keys of other applications or programs and keys that
 * do not have the format of this class are never read, counted or removed. The namespace is never
 * cleared. Nothing secret is logged. An exception of the storage or the namespace is logged by its
 * class name only.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
final class BBjNamespaceTokenStore implements BBjTokenStore {
  private static final Logger logger = System.getLogger(BBjNamespaceTokenStore.class.getName());
  private static final String HANDLE_KEY = "webforj-bbj-handle";
  private static final String KEY_KEY = "webforj-bbj-key";
  private static final String KEY_PREFIX = "webforj.bbj-security-tokens.";
  private static final String VERSION = "v1.";
  private static final String NOT_KEPT = "The sign-in could not be kept";
  private static final String FAILURE = "{0} ({1})";
  private static final Pattern ENTRY_NAME = Pattern.compile("v1\\.(\\d{1,15})\\.[A-Za-z0-9_-]{43}");
  private static final Pattern UNSAFE_CHARACTERS = Pattern.compile("[^A-Za-z0-9_-]");
  private static final int MAX_APPLICATION_NAME_LENGTH = 64;
  private static final int MAX_ENTRIES = 10000;

  private final Supplier<GlobalNamespace> namespaceSupplier;
  private final Supplier<String> applicationNameSupplier;
  private final Supplier<SessionStorage> storageSupplier;
  private final Clock clock;
  private final BBjTokenEntryCipher cipher = new BBjTokenEntryCipher();
  private GlobalNamespace namespace;
  private String applicationPrefix;

  BBjNamespaceTokenStore(Supplier<GlobalNamespace> namespaceSupplier,
      Supplier<String> applicationNameSupplier, Supplier<SessionStorage> storageSupplier,
      Clock clock) {
    this.namespaceSupplier = namespaceSupplier;
    this.applicationNameSupplier = applicationNameSupplier;
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

    GlobalNamespace target;
    String prefix;
    try {
      target = openNamespace();
      prefix = resolveApplicationPrefix();
    } catch (RuntimeException e) {
      logger.log(Level.WARNING, FAILURE, "The namespace could not be opened",
          e.getClass().getName());
      throw new IllegalStateException(NOT_KEPT);
    }

    Map<String, String> previous = storage.get(List.of(HANDLE_KEY, KEY_KEY));
    String key = cipher.createKey();
    String name =
        prefix + VERSION + session.getExpiresAt().toEpochMilli() + "." + cipher.createHandle();
    String entry = cipher.formatEntry(name, key, session);
    removeStaleEntries(target, prefix);

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
    if (!name.equals(previousName) && findExpiry(prefix, previousName).isPresent()
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
      Optional<Instant> expiresAt = findExpiry(resolveApplicationPrefix(), name);
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
      if (findExpiry(resolveApplicationPrefix(), name).isPresent()
          && removeEntries(openNamespace(), List.of(name)) == 0) {
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
   * Creates the part of an entry key that names one app.
   *
   * <p>
   * A name that holds a character other than a letter, a digit, an underscore or a hyphen, or that
   * is empty or longer than 64 characters, is replaced by a safe form followed by a short hash of
   * the original name, so that two apps never share entry keys. The result never holds a dot.
   * </p>
   *
   * @param applicationName the registered application name
   * @return the part of the entry key that names the app
   */
  static String createApplicationKey(String applicationName) {
    String safe = UNSAFE_CHARACTERS.matcher(applicationName).replaceAll("_");
    if (safe.equals(applicationName) && !safe.isEmpty()
        && safe.length() <= MAX_APPLICATION_NAME_LENGTH) {
      return safe;
    }

    String base = safe.isEmpty() ? "app"
        : safe.substring(0, Math.min(safe.length(), MAX_APPLICATION_NAME_LENGTH));

    return base + "-" + createHash(applicationName);
  }

  private GlobalNamespace openNamespace() {
    if (namespace == null) {
      namespace = namespaceSupplier.get();
    }

    return namespace;
  }

  private String resolveApplicationPrefix() {
    if (applicationPrefix == null) {
      applicationPrefix = KEY_PREFIX + createApplicationKey(applicationNameSupplier.get()) + ".";
    }

    return applicationPrefix;
  }

  private Optional<Instant> findExpiry(String prefix, String name) {
    if (name == null || !name.startsWith(prefix)) {
      return Optional.empty();
    }

    Matcher matcher = ENTRY_NAME.matcher(name.substring(prefix.length()));
    if (!matcher.matches()) {
      return Optional.empty();
    }

    return Optional.of(Instant.ofEpochMilli(Long.parseLong(matcher.group(1))));
  }

  private void removeStaleEntries(GlobalNamespace target, String prefix) {
    try {
      Map<String, Instant> entries = new HashMap<>();
      for (String name : target.keySet()) {
        findExpiry(prefix, name).ifPresent(expiresAt -> entries.put(name, expiresAt));
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

  private int removeEntries(GlobalNamespace target, List<String> names) {
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
