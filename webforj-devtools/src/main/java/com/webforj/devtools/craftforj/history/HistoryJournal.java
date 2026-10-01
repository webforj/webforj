package com.webforj.devtools.craftforj.history;

import com.google.gson.Gson;
import com.google.gson.JsonParseException;
import com.webforj.devtools.craftforj.history.HistoryLock.Hold;
import com.webforj.devtools.craftforj.history.model.HistoryEntry;
import com.webforj.devtools.craftforj.history.model.HistoryEntryInfo;
import com.webforj.devtools.craftforj.history.model.HistoryFileSnapshot;
import com.webforj.devtools.craftforj.history.model.HistoryIndex;
import com.webforj.devtools.craftforj.history.model.HistoryJournalInfo;
import com.webforj.devtools.craftforj.history.model.HistoryRestoreResult;
import com.webforj.devtools.craftforj.history.model.HistoryRestoreResult.Code;
import com.webforj.devtools.craftforj.source.staging.SourceHasher;
import java.io.IOException;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.nio.charset.StandardCharsets;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.attribute.PosixFilePermission;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Stream;

/**
 * The undo and redo journal of one application, kept on disk.
 *
 * <p>
 * An undo or a redo is refused when a file no longer holds what the step left, so a change made
 * outside craftforJ is never overwritten. A journal file that cannot be parsed is set aside and the
 * history starts empty.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class HistoryJournal {

  private static final Logger LOGGER = System.getLogger(HistoryJournal.class.getName());
  private static final Gson GSON = new Gson();
  private static final int LIMIT = 100;
  private static final long LOCK_TIMEOUT_MILLIS = 10_000;
  private static final int KEY_LENGTH = 16;
  private static final String INDEX_FILE = "journal.json";
  private static final String SEQUENCE_FILE = "sequence";
  private static final String SNAPSHOT_DIRECTORY = "snapshots";
  private static final String CORRUPT_SUFFIX = ".corrupt-";
  private static final String TEMP_SUFFIX = ".tmp";

  private final Path directory;
  private final int limit;
  private final long lockTimeout;
  private final HistoryLock lock;

  HistoryJournal(Path directory) {
    this(directory, LIMIT, LOCK_TIMEOUT_MILLIS);
  }

  HistoryJournal(Path directory, int limit, long lockTimeout) {
    this.directory = directory;
    this.limit = limit;
    this.lockTimeout = lockTimeout;
    this.lock = new HistoryLock(directory);
  }

  /**
   * Creates the journal of an application.
   *
   * @param home the home directory the journal is kept under
   * @param projectRoot the project root of the application
   * @param application the application class
   * @return the journal
   */
  public static HistoryJournal create(Path home, Path projectRoot, Class<?> application) {
    HistoryJournal journal =
        new HistoryJournal(resolveDirectory(home, projectRoot, application.getName()));
    journal.clearTemporaryFiles();

    return journal;
  }

  /**
   * Opens a step for the current thread. Every project file written until the step is closed
   * belongs to it.
   *
   * @return the step
   */
  public HistoryStep openStep() {
    HistoryStep step = new HistoryStep(this);
    ProjectFileWriter.setStep(step);

    return step;
  }

  /**
   * Gets the journal as the panel receives it.
   *
   * @return the journal, or {@code null} when it is busy or cannot be read
   */
  public synchronized HistoryJournalInfo getInfo() {
    try (Hold hold = createHold()) {
      return toInfo(new HistoryEntries(readIndex()));
    } catch (IOException e) {
      LOGGER.log(Level.WARNING, "The history is busy or could not be read", e);
      return null;
    }
  }

  /**
   * Takes an applied step back.
   *
   * @param entryId the id of the step, or {@code null} for the newest applied one
   * @return the outcome
   */
  public synchronized HistoryRestoreResult undo(Long entryId) {
    return applyStep(entryId, true);
  }

  /**
   * Writes an undone step again.
   *
   * @param entryId the id of the step, or {@code null} for the oldest undone one
   * @return the outcome
   */
  public synchronized HistoryRestoreResult redo(Long entryId) {
    return applyStep(entryId, false);
  }

  /**
   * Removes steps, so they are no longer offered.
   *
   * @param entryIds the ids of the steps
   * @return the journal afterwards with the ids it removed, or {@code null} when it is busy or
   *         cannot be read
   */
  public synchronized HistoryJournalInfo remove(Collection<Long> entryIds) {
    try (Hold hold = createHold()) {
      HistoryEntries entries = new HistoryEntries(readIndex());
      HistoryIndex index = entries.getIndex();
      List<Long> removed = entries.removeEntries(entryIds);
      if (!removed.isEmpty()) {
        writeIndex(index);
        removeUnusedSnapshots(index);
      }

      return new HistoryJournalInfo(index.getJournalId(), entries.getSteps(), removed);
    } catch (IOException e) {
      LOGGER.log(Level.WARNING, "Removing history entries failed", e);
      return null;
    }
  }

  static Path resolveDirectory(Path home, Path projectRoot, String application) {
    String root = projectRoot.toAbsolutePath().normalize().toString();
    String key = SourceHasher.hash(root + "\n" + application).substring(0, KEY_LENGTH);

    return home.resolve(".webforj").resolve("devtools").resolve("history").resolve(key);
  }

  static byte[] readFile(Path file) throws IOException {
    return Files.isRegularFile(file) ? Files.readAllBytes(file) : null;
  }

  /**
   * Removes the temporary files an interrupted write left behind, once per directory.
   */
  synchronized void clearTemporaryFiles() {
    if (lock.isCleaned()) {
      return;
    }

    Hold hold;
    try {
      hold = lock.createHold(0);
    } catch (IOException e) {
      LOGGER.log(Level.DEBUG, "The history is in use, its temporary files are removed later", e);
      return;
    }

    try (hold) {
      if (lock.isCleaned()) {
        return;
      }

      removeTemporaryFiles(directory, "", TEMP_SUFFIX);
      removeTemporaryFiles(directory.resolve(SNAPSHOT_DIRECTORY), "", TEMP_SUFFIX);
      for (Path folder : getProjectFolders(readIndex())) {
        removeTemporaryFiles(folder, ProjectFileWriter.TEMP_PREFIX, ProjectFileWriter.TEMP_SUFFIX);
      }

      lock.setCleaned(true);
      Path setAside = findSetAsideJournal();
      if (setAside != null) {
        LOGGER.log(Level.INFO,
            "Unused history snapshots are kept until " + setAside + " is removed");
      }
    } catch (IOException e) {
      LOGGER.log(Level.WARNING, "Could not remove the temporary files of the history", e);
    }
  }

  Hold createHold() throws IOException {
    return lock.createHold(lockTimeout);
  }

  /**
   * Adds the files that differ from what a step captured as one entry.
   *
   * @param before the content of each file before the step, {@code null} for a missing file
   * @return the id of the entry, or {@code null} when no file differs or the journal failed
   */
  Long addEntry(Map<Path, byte[]> before) {
    try {
      List<HistoryFileSnapshot> changed = new ArrayList<>();
      for (Map.Entry<Path, byte[]> file : before.entrySet()) {
        byte[] after = readFile(file.getKey());
        if (!Arrays.equals(file.getValue(), after)) {
          changed.add(new HistoryFileSnapshot(file.getKey().toString(),
              writeSnapshot(file.getValue()), writeSnapshot(after)));
        }
      }

      if (changed.isEmpty()) {
        return null;
      }

      HistoryEntries entries = new HistoryEntries(readIndex());
      long id = entries.createId();
      entries.addEntry(new HistoryEntry(id, changed), limit);
      writeAtomic(directory.resolve(SEQUENCE_FILE),
          Long.toString(entries.getSequence()).getBytes(StandardCharsets.UTF_8));
      writeIndex(entries.getIndex());
      removeUnusedSnapshots(entries.getIndex());

      return id;
    } catch (IOException | RuntimeException e) {
      LOGGER.log(Level.WARNING, "The write happened but could not be added to the history", e);
      return null;
    }
  }

  private HistoryRestoreResult applyStep(Long entryId, boolean undo) {
    Hold hold;
    try {
      hold = createHold();
    } catch (IOException e) {
      LOGGER.log(Level.WARNING, "The history could not be opened", e);
      Code code = e instanceof HistoryBusyException ? Code.BUSY : Code.RESTORE_FAILED;

      return createRefusal(code, e.getMessage(), null, List.of(), null);
    }

    try (hold) {
      HistoryEntries entries = new HistoryEntries(readIndex());
      HistoryEntry next = undo ? entries.getUndoEntry() : entries.getRedoEntry();
      HistoryEntry entry = entryId == null ? next : entries.findEntry(entryId);
      HistoryRestoreResult refusal = findRefusal(entries, entry, undo);

      return refusal == null ? writeStep(entries, entry, undo) : refusal;
    } catch (IOException e) {
      LOGGER.log(Level.WARNING, "The history could not be read", e);
      return createRefusal(Code.RESTORE_FAILED,
          "The history could not be read, nothing was written", null, List.of(), null);
    }
  }

  private static HistoryRestoreResult findRefusal(HistoryEntries entries, HistoryEntry entry,
      boolean undo) {
    HistoryJournalInfo journal = toInfo(entries);
    if (entry == null || entry.isSkipped() || entry.isApplied() != undo) {
      Code code = undo ? Code.NOTHING_TO_UNDO : Code.NOTHING_TO_REDO;
      return createRefusal(code, "There is no step to take", null, List.of(), journal);
    }

    HistoryEntryInfo info = new HistoryEntryInfo(entry);
    HistoryEntry next = undo ? entries.getUndoEntry() : entries.getRedoEntry();
    if (entry != next) {
      List<String> first = getPaths(next);
      return createRefusal(Code.LATER_STEP,
          "Another step comes first, take it first, " + String.join(", ", first), info, first,
          journal);
    }

    List<String> blocked = entries.findBlockedFiles(entry);
    if (!blocked.isEmpty()) {
      return createRefusal(Code.LATER_STEP,
          "A later step changed these files again, take it back first, "
              + String.join(", ", blocked),
          info, blocked, journal);
    }

    List<String> changed = findChangedFiles(entries, entry);
    if (!changed.isEmpty()) {
      return createRefusal(Code.FILE_CHANGED,
          "Changed outside craftforJ since the step was recorded, " + String.join(", ", changed),
          info, changed, journal);
    }

    return null;
  }

  private HistoryRestoreResult writeStep(HistoryEntries entries, HistoryEntry entry, boolean undo) {
    HistoryEntryInfo info = new HistoryEntryInfo(entry);
    List<String> files = getPaths(entry);
    Map<Path, byte[]> target = readSnapshots(entry, undo);
    if (target == null) {
      return createRefusal(Code.SNAPSHOT_MISSING, "The stored content of a file is gone", info,
          files, toInfo(entries));
    }

    Map<Path, byte[]> current = readFiles(target.keySet());
    if (current == null) {
      return createRefusal(Code.RESTORE_FAILED, "The files could not be read before the step", info,
          files, toInfo(entries));
    }

    Map<Path, Set<PosixFilePermission>> modes = readPermissions(current.keySet());
    List<Path> written = new ArrayList<>();
    try {
      for (Map.Entry<Path, byte[]> file : target.entrySet()) {
        ProjectFileWriter.reset(file.getKey(), file.getValue(), null);
        written.add(file.getKey());
      }

      entry.setApplied(!undo);
      writeIndex(entries.getIndex());
    } catch (IOException | RuntimeException e) {
      List<String> kept = resetFiles(written, current, modes);
      LOGGER.log(Level.WARNING, "Restoring a history step failed", e);
      if (kept.isEmpty()) {
        return createRefusal(Code.RESTORE_FAILED,
            "The step could not be completed, every file holds what it held before", info, files,
            readInfo());
      }

      return createRefusal(Code.RESTORE_INCOMPLETE,
          "The step could not be completed and these files could not be put back: "
              + String.join(", ", kept),
          info, kept, readInfo());
    }

    return HistoryRestoreResult.Builder.create().setDone(true).setEntry(new HistoryEntryInfo(entry))
        .setFiles(files).setJournal(toInfo(entries)).build();
  }

  private static HistoryRestoreResult createRefusal(Code code, String message,
      HistoryEntryInfo entry, List<String> files, HistoryJournalInfo journal) {
    return HistoryRestoreResult.Builder.create().setCode(code).setMessage(message).setEntry(entry)
        .setFiles(files).setJournal(journal).build();
  }

  private static HistoryJournalInfo toInfo(HistoryEntries entries) {
    return new HistoryJournalInfo(entries.getIndex().getJournalId(), entries.getSteps());
  }

  private HistoryJournalInfo readInfo() {
    try {
      return toInfo(new HistoryEntries(readIndex()));
    } catch (IOException e) {
      LOGGER.log(Level.WARNING, "The history could not be read", e);
      return null;
    }
  }

  private static List<String> getPaths(HistoryEntry entry) {
    return entry.getFiles().stream().map(HistoryFileSnapshot::getPath).toList();
  }

  private static List<String> findChangedFiles(HistoryEntries entries, HistoryEntry entry) {
    List<String> changed = new ArrayList<>();
    for (HistoryFileSnapshot file : entry.getFiles()) {
      if (!isHolding(Path.of(file.getPath()), entries.findLinkedContent(entry, file))) {
        changed.add(file.getPath());
      }
    }

    return changed;
  }

  private static boolean isHolding(Path file, Set<String> expected) {
    try {
      byte[] content = readFile(file);
      return expected.contains(content == null ? null : SourceHasher.hash(content));
    } catch (IOException e) {
      return false;
    }
  }

  private Map<Path, byte[]> readSnapshots(HistoryEntry entry, boolean before) {
    Map<Path, byte[]> contents = new LinkedHashMap<>();
    for (HistoryFileSnapshot file : entry.getFiles()) {
      String hash = before ? file.getBefore() : file.getAfter();
      byte[] content = null;
      if (hash != null) {
        try {
          content = Files.readAllBytes(getSnapshotPath(hash));
        } catch (IOException e) {
          return null;
        }
      }

      contents.put(Path.of(file.getPath()), content);
    }

    return contents;
  }

  private static Map<Path, byte[]> readFiles(Collection<Path> files) {
    Map<Path, byte[]> contents = new LinkedHashMap<>();
    try {
      for (Path file : files) {
        contents.put(file.toAbsolutePath().normalize(), readFile(file));
      }
    } catch (IOException e) {
      LOGGER.log(Level.WARNING, "Could not read the files before a step", e);
      return null;
    }

    return contents;
  }

  private static Map<Path, Set<PosixFilePermission>> readPermissions(Collection<Path> files) {
    Map<Path, Set<PosixFilePermission>> modes = new LinkedHashMap<>();
    for (Path file : files) {
      ProjectFileWriter.readPermissions(file).ifPresent(mode -> modes.put(file, mode));
    }

    return modes;
  }

  private static List<String> resetFiles(List<Path> files, Map<Path, byte[]> content,
      Map<Path, Set<PosixFilePermission>> modes) {
    List<String> kept = new ArrayList<>();
    for (Path file : files) {
      try {
        ProjectFileWriter.reset(file, content.get(file), modes.get(file));
      } catch (IOException | RuntimeException e) {
        LOGGER.log(Level.WARNING, "Could not put back " + file, e);
        kept.add(file.toString());
      }
    }

    return kept;
  }

  private String writeSnapshot(byte[] content) throws IOException {
    if (content == null) {
      return null;
    }

    String hash = SourceHasher.hash(content);
    Path target = getSnapshotPath(hash);
    if (!Files.exists(target)) {
      writeAtomic(target, content);
    }

    return hash;
  }

  private Path getSnapshotPath(String hash) {
    return directory.resolve(SNAPSHOT_DIRECTORY).resolve(hash);
  }

  private void removeUnusedSnapshots(HistoryIndex index) {
    Path snapshots = directory.resolve(SNAPSHOT_DIRECTORY);
    try {
      if (!Files.isDirectory(snapshots) || findSetAsideJournal() != null) {
        return;
      }

      Set<String> used = new HashSet<>();
      for (HistoryEntry entry : index.getEntries()) {
        for (HistoryFileSnapshot file : entry.getFiles()) {
          used.add(file.getBefore());
          used.add(file.getAfter());
        }
      }

      try (Stream<Path> stored = Files.list(snapshots)) {
        for (Path snapshot : stored.toList()) {
          if (!used.contains(snapshot.getFileName().toString())) {
            Files.deleteIfExists(snapshot);
          }
        }
      }
    } catch (IOException | RuntimeException e) {
      LOGGER.log(Level.WARNING, "Unused history snapshots could not be removed", e);
    }
  }

  private Path findSetAsideJournal() throws IOException {
    if (!Files.isDirectory(directory)) {
      return null;
    }

    try (Stream<Path> files = Files.list(directory)) {
      return files
          .filter(file -> file.getFileName().toString().startsWith(INDEX_FILE + CORRUPT_SUFFIX))
          .findFirst().orElse(null);
    }
  }

  private static Set<Path> getProjectFolders(HistoryIndex index) {
    Set<Path> folders = new HashSet<>();
    for (HistoryEntry entry : index.getEntries()) {
      for (HistoryFileSnapshot snapshot : entry.getFiles()) {
        Path file = Path.of(snapshot.getPath());
        addParent(folders, file);
        if (Files.isSymbolicLink(file)) {
          try {
            addParent(folders, file.toRealPath());
          } catch (IOException e) {
            LOGGER.log(Level.DEBUG, "Could not resolve the link " + file, e);
          }
        }
      }
    }

    return folders;
  }

  private static void addParent(Set<Path> folders, Path file) {
    Path parent = file.getParent();
    if (parent != null) {
      folders.add(parent);
    }
  }

  private static void removeTemporaryFiles(Path folder, String prefix, String suffix)
      throws IOException {
    if (!Files.isDirectory(folder)) {
      return;
    }

    try (Stream<Path> files = Files.list(folder)) {
      for (Path file : files.toList()) {
        String name = file.getFileName().toString();
        if (name.startsWith(prefix) && name.endsWith(suffix) && Files.isRegularFile(file)) {
          removeFile(file);
        }
      }
    }
  }

  private static void removeFile(Path file) {
    try {
      Files.deleteIfExists(file);
    } catch (IOException e) {
      LOGGER.log(Level.WARNING, "Could not remove " + file, e);
    }
  }

  private HistoryIndex readIndex() throws IOException {
    HistoryIndex index = readStoredIndex();
    if (index.getEntries() == null) {
      index.setEntries(new ArrayList<>());
    }

    index.getEntries().removeIf(entry -> !isReadable(entry));
    index.setSequence(Math.max(index.getSequence(), readSequence()));

    return index;
  }

  private HistoryIndex readStoredIndex() throws IOException {
    Path file = directory.resolve(INDEX_FILE);
    if (!Files.exists(file)) {
      return new HistoryIndex();
    }

    try {
      HistoryIndex index =
          GSON.fromJson(Files.readString(file, StandardCharsets.UTF_8), HistoryIndex.class);
      return index == null ? new HistoryIndex() : index;
    } catch (JsonParseException e) {
      Path aside = directory.resolve(INDEX_FILE + CORRUPT_SUFFIX + System.currentTimeMillis());
      Files.move(file, aside, StandardCopyOption.REPLACE_EXISTING);
      LOGGER.log(Level.WARNING, "The history journal is not valid, it is kept as " + aside
          + " and the history starts empty", e);

      return new HistoryIndex();
    }
  }

  private long readSequence() {
    try {
      Path file = directory.resolve(SEQUENCE_FILE);
      return Files.exists(file) ? Long.parseLong(Files.readString(file).trim()) : 0;
    } catch (IOException | RuntimeException e) {
      LOGGER.log(Level.DEBUG, "The history sequence file could not be read", e);
      return 0;
    }
  }

  private static boolean isReadable(HistoryEntry entry) {
    return entry != null && entry.getFiles() != null
        && entry.getFiles().stream().allMatch(file -> file != null && file.getPath() != null);
  }

  private void writeIndex(HistoryIndex index) throws IOException {
    if (index.getJournalId() == null) {
      index.setJournalId(UUID.randomUUID().toString());
    }

    writeAtomic(directory.resolve(INDEX_FILE), GSON.toJson(index).getBytes(StandardCharsets.UTF_8));
  }

  private static void writeAtomic(Path file, byte[] content) throws IOException {
    Files.createDirectories(file.getParent());
    Path temp = Files.createTempFile(file.getParent(), file.getFileName().toString(), TEMP_SUFFIX);
    try {
      Files.write(temp, content);
      try {
        Files.move(temp, file, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE);
      } catch (AtomicMoveNotSupportedException e) {
        Files.move(temp, file, StandardCopyOption.REPLACE_EXISTING);
      }
    } finally {
      removeFile(temp);
    }
  }
}
