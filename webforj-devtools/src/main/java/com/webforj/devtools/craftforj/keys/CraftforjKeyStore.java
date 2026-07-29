package com.webforj.devtools.craftforj.keys;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import java.io.IOException;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.lang.reflect.Type;
import java.nio.charset.StandardCharsets;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Server-side persistent store for developer secrets such as AI provider API keys.
 *
 * <p>
 * Entries are keyed by an arbitrary id and persisted as JSON under the developer's home directory.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class CraftforjKeyStore {

  private static final Logger LOGGER = System.getLogger(CraftforjKeyStore.class.getName());
  private static final Gson GSON = new Gson();
  private static final Type MAP_TYPE = new TypeToken<Map<String, String>>() {}.getType();
  private static volatile CraftforjKeyStore defaultInstance;

  private final Path file;
  private final Map<String, String> entries = new LinkedHashMap<>();

  /**
   * Creates a store backed by the given file. The file is loaded eagerly; a missing or unreadable
   * file simply yields an empty store.
   *
   * @param file the JSON file holding the secrets
   */
  CraftforjKeyStore(Path file) {
    this.file = file;
    load();
  }

  /**
   * Gets the default store, backed by {@code ~/.webforj/devtools/keys.json}. The same instance is
   * shared across every registration in the JVM.
   *
   * @return the default store
   */
  public static CraftforjKeyStore create() {
    CraftforjKeyStore instance = defaultInstance;
    if (instance == null) {
      synchronized (CraftforjKeyStore.class) {
        instance = defaultInstance;
        if (instance == null) {
          Path home = Path.of(System.getProperty("user.home"));
          instance = new CraftforjKeyStore(
              home.resolve(".webforj").resolve("devtools").resolve("keys.json"));
          defaultInstance = instance;
        }
      }
    }

    return instance;
  }

  /**
   * Stores or replaces a secret.
   *
   * @param id the entry id
   * @param value the secret value
   */
  public synchronized void set(String id, String value) {
    entries.put(id, value);
    persist();
  }

  /**
   * Removes a secret. Removing a missing entry is not an error.
   *
   * @param id the entry id
   */
  public synchronized void remove(String id) {
    if (entries.remove(id) != null) {
      persist();
    }
  }

  /**
   * Gets all stored secrets.
   *
   * @return a copy of the entries, keyed by id
   */
  public synchronized Map<String, String> getAll() {
    return new LinkedHashMap<>(entries);
  }

  /**
   * Gets the ids of all stored secrets, without their values.
   *
   * @return the entry ids
   */
  public synchronized List<String> getIds() {
    return new ArrayList<>(entries.keySet());
  }

  private void load() {
    if (!Files.isRegularFile(file)) {
      return;
    }

    try {
      String json = Files.readString(file, StandardCharsets.UTF_8);
      Map<String, String> loaded = GSON.fromJson(json, MAP_TYPE);
      if (loaded != null) {
        entries.putAll(loaded);
      }
    } catch (IOException | RuntimeException e) {
      LOGGER.log(Level.WARNING, "Failed to load craftforJ key store: {0}", e.getMessage());
    }
  }

  /**
   * Writes the entries to an owner-only temp file and moves it over the store atomically.
   */
  private void persist() {
    try {
      Files.createDirectories(file.getParent());
      Path temp = file.resolveSibling(file.getFileName() + ".tmp");
      writeOwnerOnly(temp, GSON.toJson(entries));
      try {
        Files.move(temp, file, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING);
      } catch (AtomicMoveNotSupportedException e) {
        Files.move(temp, file, StandardCopyOption.REPLACE_EXISTING);
      }
    } catch (IOException e) {
      LOGGER.log(Level.WARNING, "Failed to persist craftforJ key store: {0}", e.getMessage());
    }
  }

  private void writeOwnerOnly(Path target, String content) throws IOException {
    Files.deleteIfExists(target);
    try {
      Files.createFile(target,
          PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rw-------")));
    } catch (UnsupportedOperationException e) {
      Files.createFile(target);
    }
    Files.writeString(target, content, StandardCharsets.UTF_8);
  }
}
