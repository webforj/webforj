package com.webforj.devtools.craftforj.inspector.source.staging;

import com.webforj.devtools.craftforj.inspector.source.staging.model.StagedFile;
import com.webforj.environment.ObjectTable;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Supplier;

/**
 * Session scoped set of compile validated source files waiting for user approval.
 *
 * <p>
 * Staging never touches disk. Apply is atomic, pre images of every touched file are snapshotted
 * first and every snapshot is restored when any single write fails, so a partial apply can never
 * survive.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class SourceStagingArea {

  private static final String STORAGE_KEY = SourceStagingArea.class.getName();

  private final Supplier<Map<String, StagedFile>> storage;

  /**
   * Creates a staging area backed by the current webforJ session.
   */
  public SourceStagingArea() {
    this(SourceStagingArea::getSessionStorage);
  }

  /**
   * Creates a staging area with custom storage.
   *
   * @param storage supplies the backing map
   */
  public SourceStagingArea(Supplier<Map<String, StagedFile>> storage) {
    this.storage = storage;
  }

  /**
   * Puts a file into the staging area, replacing any previous entry for the same path.
   *
   * @param file the staged file
   */
  public void stage(StagedFile file) {
    storage.get().put(normalize(file.getPath()), file);
  }

  /**
   * Gets all staged files in staging order.
   *
   * @return the staged files
   */
  public List<StagedFile> list() {
    return new ArrayList<>(storage.get().values());
  }

  /**
   * Gets one staged file.
   *
   * @param path the file path
   * @return the staged file, or {@code null} when not staged
   */
  public StagedFile get(String path) {
    return storage.get().get(normalize(path));
  }

  /**
   * Removes one staged file.
   *
   * @param path the file path
   * @return {@code true} when an entry was removed
   */
  public boolean discard(String path) {
    return storage.get().remove(normalize(path)) != null;
  }

  /**
   * Removes every staged file.
   */
  public void clear() {
    storage.get().clear();
  }

  /**
   * Writes every staged file to disk atomically.
   *
   * <p>
   * Existing files whose on disk content no longer matches the hash captured at read time abort the
   * apply before anything is written. When any write fails midway, every already written file is
   * restored from its snapshot and created files are deleted.
   * </p>
   *
   * @return the applied file paths
   * @throws StagingException when validation or a write fails, disk state is unchanged
   */
  public List<String> apply() {
    List<StagedFile> files = list();
    if (files.isEmpty()) {
      return List.of();
    }

    for (StagedFile file : files) {
      if (file.isNew()) {
        continue;
      }

      String diskHash = readDiskHash(file.getPath());
      if (diskHash == null || !diskHash.equals(file.getBaseHash())) {
        throw new StagingException("SOURCE_CHANGED",
            "File changed on disk since it was read, re-read it before applying " + file.getPath());
      }
    }

    Map<String, String> snapshots = new LinkedHashMap<>();
    List<Path> created = new ArrayList<>();
    List<String> applied = new ArrayList<>();
    try {
      for (StagedFile file : files) {
        Path target = Path.of(file.getPath());
        if (file.isNew()) {
          Files.createDirectories(target.getParent());
          created.add(target);
        } else {
          snapshots.put(file.getPath(), Files.readString(target, StandardCharsets.UTF_8));
        }

        Files.writeString(target, file.getContent(), StandardCharsets.UTF_8);
        applied.add(file.getPath());
      }
    } catch (IOException e) {
      restore(snapshots, created);
      throw new StagingException("APPLY_FAILED",
          "Applying the staged files failed and the previous content was restored, "
              + e.getMessage());
    }

    clear();

    return applied;
  }

  private static void restore(Map<String, String> snapshots, List<Path> created) {
    for (Map.Entry<String, String> snapshot : snapshots.entrySet()) {
      try {
        Files.writeString(Path.of(snapshot.getKey()), snapshot.getValue(), StandardCharsets.UTF_8);
      } catch (IOException e) {
        // Restoring a snapshot failed, remaining snapshots are still attempted
      }
    }

    for (Path path : created) {
      try {
        Files.deleteIfExists(path);
      } catch (IOException e) {
        // Removing a created file failed, remaining created files are still attempted
      }
    }
  }

  private static String readDiskHash(String path) {
    try {
      return SourceHasher.hash(Files.readString(Path.of(path), StandardCharsets.UTF_8));
    } catch (IOException e) {

      return null;
    }
  }

  private static String normalize(String path) {
    return Path.of(path).toAbsolutePath().normalize().toString();
  }

  @SuppressWarnings("unchecked")
  private static Map<String, StagedFile> getSessionStorage() {
    if (!ObjectTable.contains(STORAGE_KEY)) {
      ObjectTable.put(STORAGE_KEY, new LinkedHashMap<String, StagedFile>());
    }

    return (Map<String, StagedFile>) ObjectTable.get(STORAGE_KEY);
  }
}
