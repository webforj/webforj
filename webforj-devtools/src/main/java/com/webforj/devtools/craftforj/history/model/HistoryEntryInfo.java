package com.webforj.devtools.craftforj.history.model;

import java.util.List;

/**
 * One entry as the panel receives it.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class HistoryEntryInfo {

  private final long id;
  private final boolean applied;
  private final List<HistoryFileSnapshot> files;

  /**
   * Creates the info of an entry.
   *
   * @param entry the journal entry
   */
  public HistoryEntryInfo(HistoryEntry entry) {
    this.id = entry.getId();
    this.applied = entry.isApplied();
    this.files = entry.getFiles();
  }

  /**
   * Gets the id.
   *
   * @return the id
   */
  public long getId() {
    return id;
  }

  /**
   * Checks whether the entry is on disk.
   *
   * @return {@code true} while applied, {@code false} once undone
   */
  public boolean isApplied() {
    return applied;
  }

  /**
   * Gets the files the entry changed.
   *
   * @return the file snapshots
   */
  public List<HistoryFileSnapshot> getFiles() {
    return files;
  }
}
