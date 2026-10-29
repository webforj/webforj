package com.webforj.bbj.security.store;

import java.io.IOException;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.FileAttribute;
import java.nio.file.attribute.PosixFilePermissions;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HexFormat;
import java.util.LinkedHashMap;
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

/**
 * File of named entries of one application on the server.
 *
 * <p>
 * The file holds one line per entry with its name and its value. The name carries the expiry of the
 * entry. Lines whose name or value does not fit the {@link Content} of the file are not read and
 * are dropped by the next write. The folder and the file are readable by their owner only.
 * </p>
 *
 * <p>
 * Reads and writes of one file are serialized within the JVM and between processes. A write
 * replaces the whole file at once, so a failed write leaves the previous file in place.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
final class BBjEntryFile {
  private static final String FOLDER = "webforj-bbj-security";
  private static final char SEPARATOR = '\t';
  private static final Pattern UNSAFE_CHARACTERS = Pattern.compile("[^A-Za-z0-9_-]");
  private static final int MAX_APPLICATION_NAME_LENGTH = 64;
  private static final int APPLICATION_HASH_BYTES = 4;
  private static final int MAX_ENTRIES = 10000;
  private static final ConcurrentMap<Path, ReentrantLock> LOCKS = new ConcurrentHashMap<>();
  private final Supplier<Path> directorySupplier;
  private final Supplier<String> applicationNameSupplier;
  private final Content content;
  private String applicationKey;
  private Path file;

  BBjEntryFile(Supplier<Path> directorySupplier, Supplier<String> applicationNameSupplier,
      Content content) {
    this.directorySupplier = directorySupplier;
    this.applicationNameSupplier = applicationNameSupplier;
    this.content = content;
  }

  /**
   * Creates the part of the file name and of the browser storage names that names one application.
   *
   * <p>
   * A name that holds a character other than a letter, a digit, an underscore or a hyphen, or that
   * is empty or longer than 64 characters, is replaced by a safe form followed by a short hash of
   * the original name, so that two applications never share a file or a browser storage name.
   * </p>
   *
   * @param applicationName the registered application name
   * @return the part of the file name and of the browser storage names that names the application
   */
  static String createApplicationKey(String applicationName) {
    String safe = UNSAFE_CHARACTERS.matcher(applicationName).replaceAll("_");
    if (safe.equals(applicationName) && !safe.isEmpty()
        && safe.length() <= MAX_APPLICATION_NAME_LENGTH) {
      return safe;
    }

    String base = safe.isEmpty() ? "app"
        : safe.substring(0, Math.min(safe.length(), MAX_APPLICATION_NAME_LENGTH));

    return base + "-" + createHash(applicationName, APPLICATION_HASH_BYTES);
  }

  /**
   * Creates the hexadecimal form of the leading bytes of the SHA-256 hash of a text.
   *
   * @param text the text to hash
   * @param bytes the number of leading bytes of the hash, 1 to 32
   * @return two hexadecimal characters per byte
   *
   * @throws IllegalStateException if the JVM has no SHA-256
   */
  static String createHash(String text, int bytes) {
    try {
      byte[] digest =
          MessageDigest.getInstance("SHA-256").digest(text.getBytes(StandardCharsets.UTF_8));
      return HexFormat.of().formatHex(digest, 0, bytes);
    } catch (NoSuchAlgorithmException e) {
      throw new IllegalStateException("The JVM has no SHA-256", e);
    }
  }

  Path resolveFile() {
    if (file == null) {
      file = directorySupplier.get().resolve(FOLDER)
          .resolve(resolveApplicationKey() + content.getSuffix());
    }

    return file;
  }

  String resolveApplicationKey() {
    if (applicationKey == null) {
      applicationKey = createApplicationKey(applicationNameSupplier.get());
    }

    return applicationKey;
  }

  /**
   * Reads every entry of the file.
   *
   * @return the entries by name, empty when there is no file
   *
   * @throws IOException if the file cannot be read
   */
  Map<String, String> readEntries() throws IOException {
    Path target = resolveFile();
    ReentrantLock lock = LOCKS.computeIfAbsent(target, key -> new ReentrantLock());
    lock.lock();
    try {
      if (Files.notExists(target)) {
        return new LinkedHashMap<>();
      }

      try (FileChannel channel = FileChannel.open(target, StandardOpenOption.READ);
          FileLock ignored = channel.lock(0, Long.MAX_VALUE, true)) {
        return readFile(target);
      }
    } finally {
      lock.unlock();
    }
  }

  Optional<String> findEntry(String name) throws IOException {
    return Optional.ofNullable(readEntries().get(name));
  }

  /**
   * Changes the entries of the file and writes them back.
   *
   * <p>
   * The file and its folder are created when missing. The change gets the entries read from the
   * file and changes them in place.
   * </p>
   *
   * @param change changes the entries
   *
   * @throws IOException if the file cannot be read or written
   */
  void updateEntries(Consumer<Map<String, String>> change) throws IOException {
    Path target = resolveFile();
    ReentrantLock lock = LOCKS.computeIfAbsent(target, key -> new ReentrantLock());
    lock.lock();
    try {
      Files.createDirectories(target.getParent(), createAttributes(target, "rwx------"));
      Files.newByteChannel(target, Set.of(StandardOpenOption.CREATE, StandardOpenOption.WRITE),
          createAttributes(target, "rw-------")).close();
      try (
          FileChannel channel =
              FileChannel.open(target, StandardOpenOption.READ, StandardOpenOption.WRITE);
          FileLock ignored = channel.lock()) {
        Map<String, String> entries = readFile(target);
        change.accept(entries);
        writeEntries(target, entries);
      }
    } finally {
      lock.unlock();
    }
  }

  /**
   * Removes one entry from the file.
   *
   * @param name the name of the entry
   *
   * @throws IOException if the file cannot be read or written
   */
  void removeEntry(String name) throws IOException {
    if (Files.exists(resolveFile())) {
      updateEntries(entries -> entries.remove(name));
    }
  }

  Optional<Instant> findExpiry(String name) {
    if (name == null) {
      return Optional.empty();
    }

    Matcher matcher = content.getEntryName().matcher(name);
    if (!matcher.matches()) {
      return Optional.empty();
    }

    return Optional.of(Instant.ofEpochMilli(Long.parseLong(matcher.group(1))));
  }

  /**
   * Removes the expired entries and makes room for one more entry.
   *
   * <p>
   * At the limit of 10000 entries, the entries that expire first are removed until one more entry
   * fits.
   * </p>
   *
   * @param entries the entries to change in place
   * @param now the time the expiries are compared with
   * @return the number of entries that were removed because of the limit
   */
  int removeStaleEntries(Map<String, String> entries, Instant now) {
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

    int surplus = Math.max(0, live.size() - MAX_ENTRIES + 1);
    live.subList(0, surplus).forEach(entries::remove);

    return surplus;
  }

  int getMaxEntries() {
    return MAX_ENTRIES;
  }

  private Map<String, String> readFile(Path target) throws IOException {
    Map<String, String> entries = new LinkedHashMap<>();
    String text = new String(Files.readAllBytes(target), StandardCharsets.UTF_8);
    text.lines().forEach(line -> {
      int split = line.indexOf(SEPARATOR);
      if (split > 0) {
        String name = line.substring(0, split);
        String value = line.substring(split + 1);
        if (findExpiry(name).isPresent() && content.getEntryValue().matcher(value).matches()) {
          entries.put(name, value);
        }
      }
    });

    return entries;
  }

  private static void writeEntries(Path target, Map<String, String> entries) throws IOException {
    Path temporary = Files.createTempFile(target.getParent(), "entries", ".tmp");
    try {
      List<String> lines = new ArrayList<>();
      entries.forEach((name, value) -> lines.add(name + SEPARATOR + value));
      Files.write(temporary, lines, StandardCharsets.UTF_8);
      Files.move(temporary, target, StandardCopyOption.ATOMIC_MOVE);
    } finally {
      Files.deleteIfExists(temporary);
    }
  }

  private static FileAttribute<?>[] createAttributes(Path target, String permissions) {
    if (!target.getFileSystem().supportedFileAttributeViews().contains("posix")) {
      return new FileAttribute<?>[0];
    }

    return new FileAttribute<?>[] {
        PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString(permissions))};
  }

  /**
   * What an entry file holds, with its file suffix and the form of its entry names and values.
   *
   * @author Christian Liebhardt
   * @since 26.03
   */
  enum Content {
    /** Encrypted sign ins of the browsers. */
    SESSIONS(".sessions", "v1\\.(\\d{1,15})\\.[A-Za-z0-9_-]{43}", "[A-Za-z0-9_-]+"),

    /** Counters of failed sign in attempts. */
    COUNTERS(".counters", "t2\\.(\\d{1,15})\\.\\d{1,15}\\.[ua]\\.[0-9a-f]{32}", "\\d{1,9}");

    private final String suffix;
    private final Pattern entryName;
    private final Pattern entryValue;

    Content(String suffix, String entryName, String entryValue) {
      this.suffix = suffix;
      this.entryName = Pattern.compile(entryName);
      this.entryValue = Pattern.compile(entryValue);
    }

    String getSuffix() {
      return suffix;
    }

    Pattern getEntryName() {
      return entryName;
    }

    Pattern getEntryValue() {
      return entryValue;
    }
  }
}
