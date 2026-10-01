package com.webforj.devtools.craftforj.history.model;

import java.util.List;

/**
 * The journal as the panel receives it.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class HistoryJournalInfo {

  private final String journalId;
  private final List<HistoryEntryInfo> entries;
  private final List<Long> removed;

  /**
   * Creates the info of a journal.
   *
   * @param journalId the identity of the journal, or {@code null} while it was never written
   * @param entries the entries, newest first
   */
  public HistoryJournalInfo(String journalId, List<HistoryEntryInfo> entries) {
    this.journalId = journalId;
    this.entries = List.copyOf(entries);
    this.removed = null;
  }

  /**
   * Creates the info of a journal after a remove.
   *
   * @param journalId the identity of the journal, or {@code null} while it was never written
   * @param entries the entries, newest first
   * @param removed the ids of the entries the remove took out
   */
  public HistoryJournalInfo(String journalId, List<HistoryEntryInfo> entries, List<Long> removed) {
    this.journalId = journalId;
    this.entries = List.copyOf(entries);
    this.removed = List.copyOf(removed);
  }

  /**
   * Gets the identity of the journal, which changes when the journal is replaced.
   *
   * @return the identity, or {@code null} while the journal was never written
   */
  public String getJournalId() {
    return journalId;
  }

  /**
   * Gets the entries.
   *
   * @return the entries, newest first
   */
  public List<HistoryEntryInfo> getEntries() {
    return entries;
  }

  /**
   * Gets the ids a remove took out of the journal.
   *
   * @return the ids, or {@code null} when the info does not come from a remove
   */
  public List<Long> getRemoved() {
    return removed;
  }
}
