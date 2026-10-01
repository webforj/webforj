package com.webforj.devtools.craftforj.history.model;

import java.util.ArrayList;
import java.util.List;

/**
 * The journal as it is kept on disk.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class HistoryIndex {

  private String journalId;
  private long sequence;
  private List<HistoryEntry> entries = new ArrayList<>();

  /**
   * Gets the identity of the journal.
   *
   * @return the identity, or {@code null} while the journal was never written
   */
  public String getJournalId() {
    return journalId;
  }

  /**
   * Sets the identity of the journal.
   *
   * @param journalId the identity
   */
  public void setJournalId(String journalId) {
    this.journalId = journalId;
  }

  /**
   * Gets the highest id the journal gave out.
   *
   * @return the id, {@code 0} while the journal gave none out
   */
  public long getSequence() {
    return sequence;
  }

  /**
   * Sets the highest id the journal gave out.
   *
   * @param sequence the id
   */
  public void setSequence(long sequence) {
    this.sequence = sequence;
  }

  /**
   * Gets the entries.
   *
   * @return the entries, oldest first
   */
  public List<HistoryEntry> getEntries() {
    return entries;
  }

  /**
   * Sets the entries.
   *
   * @param entries the entries, oldest first
   */
  public void setEntries(List<HistoryEntry> entries) {
    this.entries = entries;
  }
}
