package com.webforj.bbj.security.store;

import com.webforj.bbj.security.model.BBjPrincipal;
import com.webforj.bbj.security.model.BBjSession;
import com.webforj.router.history.Location;
import com.webforj.router.security.AbstractRouteSecurityManager;
import com.webforj.webstorage.LocalStorage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.nio.charset.StandardCharsets;
import java.security.GeneralSecurityException;
import java.security.SecureRandom;
import java.time.Clock;
import java.time.Instant;
import java.util.Arrays;
import java.util.Base64;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Supplier;
import javax.crypto.Cipher;
import javax.crypto.spec.GCMParameterSpec;
import javax.crypto.spec.SecretKeySpec;

/**
 * {@link BBjSecurityStore} that keeps the session of each signed in browser encrypted in one file
 * per application and the requested location in the local storage of the browser.
 *
 * <p>
 * The file holds one line per signed in browser with the name of the entry and the encrypted
 * session. The local storage of the browser holds the name of the entry and the key that opens it,
 * under names that carry the application, so the file never holds the key and the browser never
 * holds the token. All tabs of one browser share the sign in and the requested location. A new key
 * and a new name are made on every save.
 * </p>
 *
 * <p>
 * Every save removes expired entries and, at the limit of 10000 entries, the entries that expire
 * first. Lines that are not entries are dropped by the next write. Nothing secret is logged. A
 * failure is logged by its class name only.
 * </p>
 *
 * <p>
 * Only paths inside the app are read back as the requested location. Any other stored value is
 * discarded. When the local storage is not available no location is kept. Failures of the location
 * are logged and never thrown.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
final class BBjBrowserSecurityStore implements BBjSecurityStore {
  private static final Logger logger = System.getLogger(BBjBrowserSecurityStore.class.getName());
  private static final String LOCATION_KEY = AbstractRouteSecurityManager.PRE_AUTH_LOCATION_KEY;
  private static final String HANDLE_KEY_PREFIX = "webforj-bbj-handle.";
  private static final String KEY_KEY_PREFIX = "webforj-bbj-key.";
  private static final String VERSION = "v1.";
  private static final String NOT_KEPT = "The sign in could not be kept";
  private static final String FAILURE = "{0} ({1})";
  private final BBjEntryFile file;
  private final Supplier<LocalStorage> storageSupplier;
  private final Clock clock;
  private final BBjSessionEntryCipher cipher = new BBjSessionEntryCipher();

  BBjBrowserSecurityStore(BBjEntryFile file, Supplier<LocalStorage> storageSupplier, Clock clock) {
    this.file = file;
    this.storageSupplier = storageSupplier;
    this.clock = clock;
  }

  BBjBrowserSecurityStore(BBjEntryFile file, Supplier<LocalStorage> storageSupplier) {
    this(file, storageSupplier, Clock.systemUTC());
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void saveSession(BBjSession session) {
    LocalStorage storage = storageSupplier.get();
    if (storage == null) {
      throw new IllegalStateException(NOT_KEPT);
    }

    try {
      file.resolveFile();
    } catch (RuntimeException e) {
      logger.log(Level.WARNING, FAILURE, "The session file could not be opened",
          e.getClass().getName());
      throw new IllegalStateException(NOT_KEPT);
    }

    String handleKey = resolveStorageKey(HANDLE_KEY_PREFIX);
    String keyKey = resolveStorageKey(KEY_KEY_PREFIX);
    Map<String, String> previous = storage.get(List.of(handleKey, keyKey));
    String key = cipher.createKey();
    String name = VERSION + session.getExpiresAt().toEpochMilli() + "." + cipher.createHandle();
    String entry = cipher.formatEntry(name, key, session);

    try {
      file.updateEntries(entries -> {
        removeStaleEntries(entries);
        entries.put(name, entry);
      });
    } catch (IOException | RuntimeException e) {
      logger.log(Level.WARNING, FAILURE, "The sign in could not be written to the session file",
          e.getClass().getName());
      throw new IllegalStateException(NOT_KEPT);
    }

    try {
      storage.add(Map.of(handleKey, name, keyKey, key));
    } catch (RuntimeException e) {
      logger.log(Level.WARNING, FAILURE, "The sign in could not be written to the browser",
          e.getClass().getName());
      removeEntry(name);
      resetPair(storage, previous, handleKey, keyKey);
      throw new IllegalStateException(NOT_KEPT);
    }

    String previousName = previous.get(handleKey);
    if (!name.equals(previousName) && file.findExpiry(previousName).isPresent()) {
      removeEntry(previousName);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Optional<BBjSession> readSession() {
    try {
      LocalStorage storage = storageSupplier.get();
      if (storage == null) {
        return Optional.empty();
      }

      String handleKey = resolveStorageKey(HANDLE_KEY_PREFIX);
      String keyKey = resolveStorageKey(KEY_KEY_PREFIX);
      Map<String, String> pair = storage.get(List.of(handleKey, keyKey));
      if (pair.isEmpty()) {
        return Optional.empty();
      }

      String name = pair.get(handleKey);
      String key = pair.get(keyKey);
      Optional<Instant> expiresAt = file.findExpiry(name);
      if (key == null || expiresAt.isEmpty() || !expiresAt.get().isAfter(clock.instant())) {
        logger.log(Level.DEBUG, "A stored sign in is not usable and was removed");
        clearSession();
        return Optional.empty();
      }

      Optional<String> value = file.findEntry(name);
      if (value.isEmpty()) {
        logger.log(Level.DEBUG, "A stored sign in has no entry and was removed");
        clearSession();
        return Optional.empty();
      }

      Optional<BBjSession> session = cipher.parseEntry(name, key, value.get());
      if (session.isEmpty()) {
        logger.log(Level.WARNING, "A stored sign in could not be read and was removed");
        clearSession();
      }

      return session;
    } catch (IOException | RuntimeException e) {
      logger.log(Level.WARNING, FAILURE, "The stored sign in could not be read",
          e.getClass().getName());
      return Optional.empty();
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void clearSession() {
    LocalStorage storage;
    String handleKey;
    String keyKey;
    try {
      storage = storageSupplier.get();
      handleKey = resolveStorageKey(HANDLE_KEY_PREFIX);
      keyKey = resolveStorageKey(KEY_KEY_PREFIX);
    } catch (RuntimeException e) {
      logger.log(Level.WARNING, FAILURE, "The browser storage could not be read",
          e.getClass().getName());
      return;
    }

    if (storage == null) {
      return;
    }

    try {
      String name = storage.get(List.of(handleKey, keyKey)).get(handleKey);
      file.findExpiry(name).ifPresent(it -> removeEntry(name));
    } catch (RuntimeException e) {
      logger.log(Level.WARNING, FAILURE, "The sign in could not be read from the browser",
          e.getClass().getName());
    }

    try {
      storage.remove(handleKey, keyKey);
    } catch (RuntimeException e) {
      logger.log(Level.WARNING, FAILURE, "The sign in could not be removed from the browser",
          e.getClass().getName());
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void saveLocation(Location location) {
    try {
      Optional.ofNullable(storageSupplier.get())
          .ifPresent(it -> it.setItem(LOCATION_KEY, location.getFullURI()));
    } catch (Exception e) {
      logger.log(Level.WARNING, "The requested location could not be saved ({0})",
          e.getClass().getName());
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Optional<Location> readLocation() {
    try {
      return Optional.ofNullable(storageSupplier.get()).map(it -> it.get(LOCATION_KEY))
          .filter(it -> {
            if (!isRelativePath(it)) {
              clearLocation();
              logger.log(Level.WARNING,
                  "Discarded a requested location that is not a path inside the app");
              return false;
            }
            return true;
          }).map(Location::new);
    } catch (Exception e) {
      logger.log(Level.WARNING, "The requested location could not be read ({0})",
          e.getClass().getName());
      return Optional.empty();
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void clearLocation() {
    try {
      Optional.ofNullable(storageSupplier.get()).ifPresent(it -> it.remove(LOCATION_KEY));
    } catch (Exception e) {
      logger.log(Level.WARNING, "The requested location could not be cleared ({0})",
          e.getClass().getName());
    }
  }

  /**
   * Tells whether a stored value is a path inside the app.
   *
   * <p>
   * A path inside the app starts with one slash, holds no backslash and is a valid location.
   * </p>
   *
   * @param path the stored value
   * @return true when the value is a path inside the app
   */
  static boolean isRelativePath(String path) {
    if (!path.startsWith("/") || path.startsWith("//") || path.indexOf('\\') >= 0) {
      return false;
    }

    try {
      new Location(path);
      return true;
    } catch (IllegalArgumentException e) {
      return false;
    }
  }

  private String resolveStorageKey(String prefix) {
    return prefix + file.resolveApplicationKey();
  }

  private void removeEntry(String name) {
    try {
      file.removeEntry(name);
    } catch (IOException | RuntimeException e) {
      logger.log(Level.WARNING, FAILURE, "The session entry could not be removed",
          e.getClass().getName());
    }
  }

  private void removeStaleEntries(Map<String, String> entries) {
    try {
      int removed = file.removeStaleEntries(entries, clock.instant());
      if (removed > 0) {
        logger.log(Level.WARNING,
            "The session store removed {0} sessions because its limit of {1} was reached",
            String.valueOf(removed), String.valueOf(file.getMaxEntries()));
      }
    } catch (RuntimeException e) {
      logger.log(Level.WARNING, FAILURE, "Expired sign ins could not be removed",
          e.getClass().getName());
    }
  }

  private void resetPair(LocalStorage storage, Map<String, String> previous, String handleKey,
      String keyKey) {
    try {
      if (previous.size() == 2) {
        storage.add(previous);
      } else {
        storage.remove(handleKey, keyKey);
      }
    } catch (RuntimeException e) {
      logger.log(Level.WARNING, FAILURE, "The previous sign in could not be restored",
          e.getClass().getName());
    }
  }

  /**
   * Encrypts a session into the text of one entry and reads it back.
   *
   * <p>
   * The text is the Base64 URL form of a random initialization vector followed by the AES-256-GCM
   * ciphertext. The name of the entry is authenticated with it, so a text moved to another entry
   * name cannot be read.
   * </p>
   *
   * @author Christian Liebhardt
   * @since 26.03
   */
  static final class BBjSessionEntryCipher {
    private static final String TRANSFORMATION = "AES/GCM/NoPadding";
    private static final int RANDOM_BYTES = 32;
    private static final int IV_BYTES = 12;
    private static final int TAG_BITS = 128;
    private static final int TAG_BYTES = TAG_BITS / 8;
    private final SecureRandom random = new SecureRandom();

    /**
     * Creates a random handle for an entry name.
     *
     * @return 43 URL safe characters
     */
    String createHandle() {
      return createRandomText();
    }

    /**
     * Creates a random key for one entry.
     *
     * @return 43 URL safe characters
     */
    String createKey() {
      return createRandomText();
    }

    /**
     * Encrypts a session into the text of an entry.
     *
     * @param entryName the name of the entry, authenticated with the text
     * @param key the key made by {@link #createKey()}
     * @param session the session to encrypt
     * @return the text of the entry
     *
     * @throws IllegalStateException if the session cannot be encrypted, the message holds no secret
     *         and there is no cause
     */
    String formatEntry(String entryName, String key, BBjSession session) {
      byte[] plain = null;
      byte[] keyBytes = null;

      try {
        keyBytes = Base64.getUrlDecoder().decode(key);
        plain = writePlain(session);
        byte[] iv = new byte[IV_BYTES];
        random.nextBytes(iv);
        Cipher cipher = Cipher.getInstance(TRANSFORMATION);
        cipher.init(Cipher.ENCRYPT_MODE, new SecretKeySpec(keyBytes, "AES"),
            new GCMParameterSpec(TAG_BITS, iv));
        cipher.updateAAD(entryName.getBytes(StandardCharsets.UTF_8));
        byte[] encrypted = cipher.doFinal(plain);
        byte[] whole = new byte[IV_BYTES + encrypted.length];
        System.arraycopy(iv, 0, whole, 0, IV_BYTES);
        System.arraycopy(encrypted, 0, whole, IV_BYTES, encrypted.length);

        return Base64.getUrlEncoder().withoutPadding().encodeToString(whole);
      } catch (GeneralSecurityException | IOException | RuntimeException e) {
        throw new IllegalStateException("The sign in could not be encrypted");
      } finally {
        clearBytes(plain);
        clearBytes(keyBytes);
      }
    }

    /**
     * Decrypts the text of an entry.
     *
     * @param entryName the name of the entry the text was made for
     * @param key the key the text was made with
     * @param entry the text of the entry
     * @return the session, or empty when the key, the name or the text is wrong or the text was
     *         changed
     */
    Optional<BBjSession> parseEntry(String entryName, String key, String entry) {
      byte[] plain = null;
      byte[] keyBytes = null;

      try {
        keyBytes = Base64.getUrlDecoder().decode(key);
        byte[] whole = Base64.getUrlDecoder().decode(entry);
        if (whole.length < IV_BYTES + TAG_BYTES) {
          return Optional.empty();
        }

        Cipher cipher = Cipher.getInstance(TRANSFORMATION);
        cipher.init(Cipher.DECRYPT_MODE, new SecretKeySpec(keyBytes, "AES"),
            new GCMParameterSpec(TAG_BITS, whole, 0, IV_BYTES));
        cipher.updateAAD(entryName.getBytes(StandardCharsets.UTF_8));
        plain = cipher.doFinal(whole, IV_BYTES, whole.length - IV_BYTES);

        return Optional.of(readPlain(plain));
      } catch (GeneralSecurityException | IOException | RuntimeException e) {
        return Optional.empty();
      } finally {
        clearBytes(plain);
        clearBytes(keyBytes);
      }
    }

    private String createRandomText() {
      byte[] bytes = new byte[RANDOM_BYTES];
      random.nextBytes(bytes);
      String text = Base64.getUrlEncoder().withoutPadding().encodeToString(bytes);
      clearBytes(bytes);

      return text;
    }

    private static byte[] writePlain(BBjSession session) throws IOException {
      ByteArrayOutputStream bytes = new ByteArrayOutputStream();
      try (DataOutputStream out = new DataOutputStream(bytes)) {
        out.writeLong(session.getExpiresAt().toEpochMilli());
        out.writeUTF(session.getPrincipal().getUserName());
        out.writeUTF(session.getToken());
        writeValues(out, session.getPrincipal().getRoles());
        writeValues(out, session.getPrincipal().getAuthorities());
      }

      return bytes.toByteArray();
    }

    private static BBjSession readPlain(byte[] plain) throws IOException {
      try (DataInputStream in = new DataInputStream(new ByteArrayInputStream(plain))) {
        Instant expiresAt = Instant.ofEpochMilli(in.readLong());
        String userName = in.readUTF();
        String token = in.readUTF();
        Set<String> roles = readValues(in);
        Set<String> authorities = readValues(in);

        return new BBjSession(new BBjPrincipal(userName, roles, authorities), token, expiresAt);
      }
    }

    private static void writeValues(DataOutputStream out, Set<String> values) throws IOException {
      out.writeInt(values.size());
      for (String value : values) {
        out.writeUTF(value);
      }
    }

    private static Set<String> readValues(DataInputStream in) throws IOException {
      int count = in.readInt();
      Set<String> values = new LinkedHashSet<>();
      for (int index = 0; index < count; index++) {
        values.add(in.readUTF());
      }

      return values;
    }

    private static void clearBytes(byte[] bytes) {
      if (bytes != null) {
        Arrays.fill(bytes, (byte) 0);
      }
    }
  }
}
