package com.webforj.devtools.craftforj.history.model;

import java.util.List;

/**
 * One request kept in the journal, with the files it changed.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class HistoryEntry {

  private final long id;
  private final List<HistoryFileSnapshot> files;
  private boolean applied = true;
  private boolean skipped;

  /**
   * Creates an applied entry.
   *
   * @param id the id, unique within the journal
   * @param files the files the request changed
   */
  public HistoryEntry(long id, List<HistoryFileSnapshot> files) {
    this.id = id;
    this.files = List.copyOf(files);
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
   * Gets the files the request changed.
   *
   * @return the file snapshots
   */
  public List<HistoryFileSnapshot> getFiles() {
    return files;
  }

  /**
   * Checks whether the request is on disk.
   *
   * @return {@code true} while applied, {@code false} once undone
   */
  public boolean isApplied() {
    return applied;
  }

  /**
   * Sets whether the request is on disk.
   *
   * @param applied {@code true} when applied, {@code false} when undone
   */
  public void setApplied(boolean applied) {
    this.applied = applied;
  }

  /**
   * Checks whether the entry is skipped, which means it is no longer offered as a step.
   *
   * @return {@code true} once skipped
   */
  public boolean isSkipped() {
    return skipped;
  }

  /**
   * Sets whether the entry is skipped.
   *
   * @param skipped {@code true} to no longer offer the entry as a step
   */
  public void setSkipped(boolean skipped) {
    this.skipped = skipped;
  }
}
