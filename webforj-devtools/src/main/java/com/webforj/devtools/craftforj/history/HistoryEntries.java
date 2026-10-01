package com.webforj.devtools.craftforj.history;

import com.webforj.devtools.craftforj.history.model.HistoryEntry;
import com.webforj.devtools.craftforj.history.model.HistoryEntryInfo;
import com.webforj.devtools.craftforj.history.model.HistoryFileSnapshot;
import com.webforj.devtools.craftforj.history.model.HistoryIndex;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * The entries of one journal, with the rules for taking, adding and skipping them.
 *
 * <p>
 * An undo takes the newest applied step and a redo the oldest undone step. A skipped entry is not a
 * step. It links the content the step before it left to the content written after it.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@SuppressWarnings("java:S6206")
final class HistoryEntries {

  private final HistoryIndex index;

  HistoryEntries(HistoryIndex index) {
    this.index = index;
  }

  HistoryIndex getIndex() {
    return index;
  }

  HistoryEntry findEntry(long id) {
    return getEntries().stream().filter(entry -> entry.getId() == id).findFirst().orElse(null);
  }

  HistoryEntry getUndoEntry() {
    List<HistoryEntry> entries = getEntries();
    for (int position = entries.size() - 1; position >= 0; position--) {
      HistoryEntry entry = entries.get(position);
      if (entry.isApplied() && !entry.isSkipped()) {
        return entry;
      }
    }

    return null;
  }

  HistoryEntry getRedoEntry() {
    return getEntries().stream().filter(entry -> !entry.isApplied() && !entry.isSkipped())
        .findFirst().orElse(null);
  }

  /**
   * Gets the entries that are steps.
   *
   * @return the steps, newest first
   */
  List<HistoryEntryInfo> getSteps() {
    List<HistoryEntry> entries = getEntries();
    List<HistoryEntryInfo> steps = new ArrayList<>();
    for (int position = entries.size() - 1; position >= 0; position--) {
      HistoryEntry entry = entries.get(position);
      if (!entry.isSkipped()) {
        steps.add(new HistoryEntryInfo(entry));
      }
    }

    return steps;
  }

  long getSequence() {
    long highest = getEntries().stream().mapToLong(HistoryEntry::getId).max().orElse(0);
    return Math.max(index.getSequence(), highest);
  }

  long createId() {
    long id = getSequence() + 1;
    index.setSequence(id);

    return id;
  }

  /**
   * Adds a step, which removes the undone steps and the oldest steps over the limit.
   *
   * @param entry the step
   * @param limit how many steps the journal keeps
   */
  void addEntry(HistoryEntry entry, int limit) {
    List<HistoryEntry> entries = getEntries();
    entries.removeIf(kept -> !kept.isSkipped() && !kept.isApplied());
    entries.add(entry);
    long steps = entries.stream().filter(kept -> !kept.isSkipped()).count();
    Iterator<HistoryEntry> iterator = entries.iterator();
    while (steps > limit && iterator.hasNext()) {
      if (!iterator.next().isSkipped()) {
        iterator.remove();
        steps--;
      }
    }

    removeUnlinkedSkipped();
  }

  /**
   * Removes the named steps. An applied step stays as a skipped entry, an undone step leaves.
   *
   * @param ids the ids of the steps
   * @return the ids of the steps that were removed
   */
  List<Long> removeEntries(Collection<Long> ids) {
    List<Long> removed =
        getEntries().stream().filter(entry -> !entry.isSkipped() && ids.contains(entry.getId()))
            .map(HistoryEntry::getId).toList();
    if (removed.isEmpty()) {
      return removed;
    }

    for (HistoryEntry entry : getEntries()) {
      if (entry.isApplied() && removed.contains(entry.getId())) {
        entry.setSkipped(true);
      }
    }

    getEntries().removeIf(entry -> !entry.isSkipped() && removed.contains(entry.getId()));
    removeUnlinkedSkipped();

    return removed;
  }

  /**
   * Finds the files of a step that a later applied step changed again.
   *
   * @param entry the step
   * @return the absolute paths
   */
  List<String> findBlockedFiles(HistoryEntry entry) {
    List<String> blocked = new ArrayList<>();
    for (HistoryFileSnapshot file : entry.getFiles()) {
      Path path = Path.of(file.getPath());
      for (HistoryEntry later : getLaterEntries(entry)) {
        if (!later.isSkipped() && later.isApplied() && findFile(later, path) != null) {
          blocked.add(file.getPath());
          break;
        }
      }
    }

    return blocked;
  }

  /**
   * Finds every content a file may hold while the step can still be taken.
   *
   * @param entry the step
   * @param file the file of the step
   * @return the content hashes
   */
  Set<String> findLinkedContent(HistoryEntry entry, HistoryFileSnapshot file) {
    Path path = Path.of(file.getPath());
    Set<String> linked = new HashSet<>();
    linked.add(entry.isApplied() ? file.getAfter() : file.getBefore());
    for (HistoryEntry later : getLaterEntries(entry)) {
      HistoryFileSnapshot skipped = later.isSkipped() ? findFile(later, path) : null;
      if (skipped != null && linked.contains(skipped.getBefore())) {
        linked.add(skipped.getAfter());
      }
    }

    return linked;
  }

  private List<HistoryEntry> getEntries() {
    return index.getEntries();
  }

  private List<HistoryEntry> getLaterEntries(HistoryEntry entry) {
    List<HistoryEntry> entries = getEntries();
    return entries.subList(entries.indexOf(entry) + 1, entries.size());
  }

  private void removeUnlinkedSkipped() {
    Set<Path> stepped = new HashSet<>();
    Iterator<HistoryEntry> iterator = getEntries().iterator();
    while (iterator.hasNext()) {
      HistoryEntry kept = iterator.next();
      Set<Path> paths = getPaths(kept);
      if (!kept.isSkipped()) {
        stepped.addAll(paths);
      } else if (paths.stream().noneMatch(stepped::contains)) {
        iterator.remove();
      }
    }

    mergeSkipped();
  }

  private void mergeSkipped() {
    List<HistoryEntry> entries = getEntries();
    int position = 0;
    while (position < entries.size()) {
      HistoryEntry skipped = entries.get(position);
      HistoryEntry earlier =
          skipped.isSkipped() ? findLastTouching(position, getPaths(skipped)) : null;
      if (earlier != null && earlier.isSkipped() && getPaths(earlier).equals(getPaths(skipped))
          && isContinuous(earlier, skipped)) {
        entries.set(position, createMerged(earlier, skipped));
        entries.remove(earlier);
      } else {
        position++;
      }
    }
  }

  private HistoryEntry findLastTouching(int position, Set<Path> paths) {
    List<HistoryEntry> entries = getEntries();
    for (int earlier = position - 1; earlier >= 0; earlier--) {
      if (getPaths(entries.get(earlier)).stream().anyMatch(paths::contains)) {
        return entries.get(earlier);
      }
    }

    return null;
  }

  private static boolean isContinuous(HistoryEntry earlier, HistoryEntry skipped) {
    for (HistoryFileSnapshot file : skipped.getFiles()) {
      HistoryFileSnapshot before = findFile(earlier, Path.of(file.getPath()));
      if (before == null || !Objects.equals(before.getAfter(), file.getBefore())) {
        return false;
      }
    }

    return true;
  }

  private static HistoryEntry createMerged(HistoryEntry earlier, HistoryEntry skipped) {
    List<HistoryFileSnapshot> files = new ArrayList<>();
    for (HistoryFileSnapshot file : skipped.getFiles()) {
      HistoryFileSnapshot before = findFile(earlier, Path.of(file.getPath()));
      files.add(new HistoryFileSnapshot(file.getPath(), before.getBefore(), file.getAfter()));
    }

    HistoryEntry merged = new HistoryEntry(skipped.getId(), files);
    merged.setSkipped(true);

    return merged;
  }

  private static HistoryFileSnapshot findFile(HistoryEntry entry, Path path) {
    return entry.getFiles().stream().filter(file -> Path.of(file.getPath()).equals(path))
        .findFirst().orElse(null);
  }

  private static Set<Path> getPaths(HistoryEntry entry) {
    return entry.getFiles().stream().map(file -> Path.of(file.getPath()))
        .collect(Collectors.toSet());
  }
}
