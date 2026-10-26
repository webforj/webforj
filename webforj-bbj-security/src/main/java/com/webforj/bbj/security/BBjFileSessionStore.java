package com.webforj.bbj.security;

import com.webforj.bbj.security.model.BBjPrincipal;
import com.webforj.bbj.security.model.BBjSession;
import com.webforj.webstorage.SessionStorage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.FileAttribute;
import java.nio.file.attribute.PosixFilePermissions;
import java.security.GeneralSecurityException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.time.Clock;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HexFormat;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.locks.ReentrantLock;
import java.util.function.Consumer;
import java.util.function.Supplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.crypto.Cipher;
import javax.crypto.spec.GCMParameterSpec;
import javax.crypto.spec.SecretKeySpec;

/**
 * Keeps the session of each signed in tab encrypted in one file per
 * application.
 *
 * <p>
 * The file holds one line per signed in tab with the name of the entry and the
 * encrypted session.
 * The session storage of the tab holds the name of the entry and the key that
 * opens it, so the file
 * never holds the key and the browser never holds the token. A new key and a
 * new name are made on
 * every save.
 * </p>
 *
 * <p>
 * Every save removes expired entries and, at the limit of 10000 entries, the
 * entries that expire
 * first. Lines that are not entries are dropped by the next write. Nothing
 * secret is logged. A
 * failure is logged by its class name only.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
public final class BBjFileSessionStore implements BBjSessionStore {
  private static final Logger logger = System.getLogger(BBjFileSessionStore.class.getName());
  private static final String HANDLE_KEY = "webforj-bbj-handle";
  private static final String KEY_KEY = "webforj-bbj-key";
  private static final String FOLDER = "webforj-bbj-security";
  private static final String SUFFIX = ".sessions";
  private static final String VERSION = "v1.";
  private static final String NOT_KEPT = "The sign in could not be kept";
  private static final String FAILURE = "{0} ({1})";
  private static final char SEPARATOR = '\t';
  private static final Pattern ENTRY_NAME = Pattern.compile("v1\\.(\\d{1,15})\\.[A-Za-z0-9_-]{43}");
  private static final Pattern ENTRY_VALUE = Pattern.compile("[A-Za-z0-9_-]+");
  private static final Pattern UNSAFE_CHARACTERS = Pattern.compile("[^A-Za-z0-9_-]");
  private static final int MAX_APPLICATION_NAME_LENGTH = 64;
  private static final int MAX_ENTRIES = 10000;
  private static final ConcurrentMap<Path, ReentrantLock> LOCKS = new ConcurrentHashMap<>();
  private final Supplier<Path> directorySupplier;
  private final Supplier<String> applicationNameSupplier;
  private final Supplier<SessionStorage> storageSupplier;
  private final Clock clock;
  private final BBjSessionEntryCipher cipher = new BBjSessionEntryCipher();
  private Path file;

  BBjFileSessionStore(Supplier<Path> directorySupplier, Supplier<String> applicationNameSupplier,
      Supplier<SessionStorage> storageSupplier, Clock clock) {
    this.directorySupplier = directorySupplier;
    this.applicationNameSupplier = applicationNameSupplier;
    this.storageSupplier = storageSupplier;
    this.clock = clock;
  }

  BBjFileSessionStore(Supplier<Path> directorySupplier, Supplier<String> applicationNameSupplier,
      Supplier<SessionStorage> storageSupplier) {
    this(directorySupplier, applicationNameSupplier, storageSupplier, Clock.systemUTC());
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void saveSession(BBjSession session) {
    SessionStorage storage = storageSupplier.get();
    if (storage == null) {
      throw new IllegalStateException(NOT_KEPT);
    }

    try {
      resolveFile();
    } catch (RuntimeException e) {
      logger.log(Level.WARNING, FAILURE, "The session file could not be opened",
          e.getClass().getName());
      throw new IllegalStateException(NOT_KEPT);
    }

    Map<String, String> previous = storage.get(List.of(HANDLE_KEY, KEY_KEY));
    String key = cipher.createKey();
    String name = VERSION + session.getExpiresAt().toEpochMilli() + "." + cipher.createHandle();
    String entry = cipher.formatEntry(name, key, session);

    try {
      updateEntries(entries -> {
        removeStaleEntries(entries);
        entries.put(name, entry);
      });
    } catch (IOException | RuntimeException e) {
      logger.log(Level.WARNING, FAILURE, "The sign in could not be written to the session file",
          e.getClass().getName());
      throw new IllegalStateException(NOT_KEPT);
    }

    try {
      storage.add(Map.of(HANDLE_KEY, name, KEY_KEY, key));
    } catch (RuntimeException e) {
      logger.log(Level.WARNING, FAILURE, "The sign in could not be written to the tab",
          e.getClass().getName());
      removeEntry(name);
      resetPair(storage, previous);
      throw new IllegalStateException(NOT_KEPT);
    }

    String previousName = previous.get(HANDLE_KEY);
    if (!name.equals(previousName) && findExpiry(previousName).isPresent()) {
      removeEntry(previousName);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Optional<BBjSession> readSession() {
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
        logger.log(Level.DEBUG, "A stored sign in is not usable and was removed");
        clearSession();
        return Optional.empty();
      }

      Optional<String> value = findEntry(name);
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
      findExpiry(name).ifPresent(it -> removeEntry(name));
    } catch (RuntimeException e) {
      logger.log(Level.WARNING, FAILURE, "The sign in could not be read from the tab",
          e.getClass().getName());
    }

    try {
      storage.remove(HANDLE_KEY, KEY_KEY);
    } catch (RuntimeException e) {
      logger.log(Level.WARNING, FAILURE, "The sign in could not be removed from the tab",
          e.getClass().getName());
    }
  }

  /**
   * Creates the part of the file name that names one application.
   *
   * <p>
   * A name that holds a character other than a letter, a digit, an underscore or
   * a hyphen, or that
   * is empty or longer than 64 characters, is replaced by a safe form followed by
   * a short hash of
   * the original name, so that two applications never share a file.
   * </p>
   *
   * @param applicationName the registered application name
   * @return the part of the file name that names the application
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

  private Path resolveFile() {
    if (file == null) {
      file = directorySupplier.get().resolve(FOLDER)
          .resolve(createApplicationKey(applicationNameSupplier.get()) + SUFFIX);
    }

    return file;
  }

  private Optional<String> findEntry(String name) throws IOException {
    Path target = resolveFile();
    ReentrantLock lock = LOCKS.computeIfAbsent(target, key -> new ReentrantLock());
    lock.lock();
    try {
      if (Files.notExists(target)) {
        return Optional.empty();
      }

      try (FileChannel channel = FileChannel.open(target, StandardOpenOption.READ);
          FileLock ignored = channel.lock(0, Long.MAX_VALUE, true)) {
        return Optional.ofNullable(readEntries(target).get(name));
      }
    } finally {
      lock.unlock();
    }
  }

  private void updateEntries(Consumer<Map<String, String>> change) throws IOException {
    Path target = resolveFile();
    ReentrantLock lock = LOCKS.computeIfAbsent(target, key -> new ReentrantLock());
    lock.lock();
    try {
      Files.createDirectories(target.getParent(), createAttributes(target, "rwx------"));
      Files.newByteChannel(target, Set.of(StandardOpenOption.CREATE, StandardOpenOption.WRITE),
          createAttributes(target, "rw-------")).close();
      try (
          FileChannel channel = FileChannel.open(target, StandardOpenOption.READ, StandardOpenOption.WRITE);
          FileLock ignored = channel.lock()) {
        Map<String, String> entries = readEntries(target);
        change.accept(entries);
        writeEntries(target, entries);
      }
    } finally {
      lock.unlock();
    }
  }

  private void removeEntry(String name) {
    try {
      if (Files.exists(resolveFile())) {
        updateEntries(entries -> entries.remove(name));
      }
    } catch (IOException | RuntimeException e) {
      logger.log(Level.WARNING, FAILURE, "The session entry could not be removed",
          e.getClass().getName());
    }
  }

  private Map<String, String> readEntries(Path file) throws IOException {
    Map<String, String> entries = new LinkedHashMap<>();
    String content = new String(Files.readAllBytes(file), StandardCharsets.UTF_8);
    content.lines().forEach(line -> {
      int split = line.indexOf(SEPARATOR);
      if (split > 0) {
        String name = line.substring(0, split);
        String value = line.substring(split + 1);
        if (findExpiry(name).isPresent() && ENTRY_VALUE.matcher(value).matches()) {
          entries.put(name, value);
        }
      }
    });

    return entries;
  }

  private void writeEntries(Path file, Map<String, String> entries) throws IOException {
    Path temporary = Files.createTempFile(file.getParent(), "sessions", ".tmp");
    try {
      List<String> lines = new ArrayList<>();
      entries.forEach((name, value) -> lines.add(name + SEPARATOR + value));
      Files.write(temporary, lines, StandardCharsets.UTF_8);
      Files.move(temporary, file, StandardCopyOption.ATOMIC_MOVE);
    } finally {
      Files.deleteIfExists(temporary);
    }
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

  private void removeStaleEntries(Map<String, String> entries) {
    try {
      Instant now = clock.instant();
      Map<String, Instant> expiries = new HashMap<>();
      entries.keySet()
          .forEach(name -> findExpiry(name).ifPresent(expiry -> expiries.put(name, expiry)));
      List<String> live = new ArrayList<>();
      expiries.forEach((name, expiresAt) -> {
        if (expiresAt.isAfter(now)) {
          live.add(name);
        } else {
          entries.remove(name);
        }
      });
      live.sort(Comparator.comparing(expiries::get));

      int surplus = live.size() - MAX_ENTRIES + 1;
      if (surplus > 0) {
        live.subList(0, surplus).forEach(entries::remove);
        logger.log(Level.WARNING,
            "The session store removed {0} sessions because its limit of {1} was reached",
            String.valueOf(surplus), String.valueOf(MAX_ENTRIES));
      }
    } catch (RuntimeException e) {
      logger.log(Level.WARNING, FAILURE, "Expired sign ins could not be removed",
          e.getClass().getName());
    }
  }

  private void resetPair(SessionStorage storage, Map<String, String> previous) {
    try {
      if (previous.size() == 2) {
        storage.add(previous);
      } else {
        storage.remove(HANDLE_KEY, KEY_KEY);
      }
    } catch (RuntimeException e) {
      logger.log(Level.WARNING, FAILURE, "The previous sign in could not be restored",
          e.getClass().getName());
    }
  }

  private static FileAttribute<?>[] createAttributes(Path target, String permissions) {
    if (!target.getFileSystem().supportedFileAttributeViews().contains("posix")) {
      return new FileAttribute<?>[0];
    }

    return new FileAttribute<?>[] {
        PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString(permissions)) };
  }

  private static String createHash(String text) {
    try {
      byte[] digest = MessageDigest.getInstance("SHA-256").digest(text.getBytes(StandardCharsets.UTF_8));
      return HexFormat.of().formatHex(digest, 0, 4);
    } catch (NoSuchAlgorithmException e) {
      throw new IllegalStateException("The JVM has no SHA-256", e);
    }
  }

  /**
   * Encrypts a session into the text of one entry and reads it back.
   *
   * <p>
   * The text is the Base64 URL form of a random initialization vector followed by
   * the AES-256-GCM
   * ciphertext. The name of the entry is authenticated with it, so a text moved
   * to another entry
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
     * @param key       the key made by {@link #createKey()}
     * @param session   the session to encrypt
     * @return the text of the entry
     *
     * @throws IllegalStateException if the session cannot be encrypted, the message
     *                               holds no secret
     *                               and there is no cause
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
     * @param key       the key the text was made with
     * @param entry     the text of the entry
     * @return the session, or empty when the key, the name or the text is wrong or
     *         the text was
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
