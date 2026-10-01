package com.webforj.devtools.craftforj.history.model;

import java.util.List;

/**
 * The outcome of an undo or a redo.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class HistoryRestoreResult {

  /**
   * Why a step was refused.
   */
  public enum Code {
    /** There is no applied step, or the named entry is not applied. */
    NOTHING_TO_UNDO,
    /** There is no undone step, or the named entry is not undone. */
    NOTHING_TO_REDO,
    /** A file changed outside craftforJ since the step was recorded. */
    FILE_CHANGED,
    /** Another step has to be taken first. */
    LATER_STEP,
    /** The stored content of a file is gone. */
    SNAPSHOT_MISSING,
    /** The step failed and every file holds what it held before. */
    RESTORE_FAILED,
    /** The step failed and some files could not be put back. */
    RESTORE_INCOMPLETE,
    /** Another writer holds the history. */
    BUSY
  }

  private final boolean done;
  private final Code code;
  private final String message;
  private final List<String> files;
  private final HistoryEntryInfo entry;
  private final HistoryJournalInfo journal;

  private HistoryRestoreResult(Builder builder) {
    this.done = builder.done;
    this.code = builder.code;
    this.message = builder.message;
    this.files = List.copyOf(builder.files);
    this.entry = builder.entry;
    this.journal = builder.journal;
  }

  /**
   * Checks whether the step ran.
   *
   * @return {@code true} when every file was restored
   */
  public boolean isDone() {
    return done;
  }

  /**
   * Gets the refusal code.
   *
   * @return the code, or {@code null} when the step ran
   */
  public Code getCode() {
    return code;
  }

  /**
   * Gets the refusal reason.
   *
   * @return the reason, or {@code null} when the step ran
   */
  public String getMessage() {
    return message;
  }

  /**
   * Gets the files the step wrote, or the files that refused it.
   *
   * @return the absolute paths
   */
  public List<String> getFiles() {
    return files;
  }

  /**
   * Gets the entry the step acted on.
   *
   * @return the entry, or {@code null} when there was none
   */
  public HistoryEntryInfo getEntry() {
    return entry;
  }

  /**
   * Gets the journal as it stands after the step.
   *
   * @return the journal, or {@code null} when it could not be read
   */
  public HistoryJournalInfo getJournal() {
    return journal;
  }

  /**
   * Builds a {@link HistoryRestoreResult}.
   *
   * @since 26.03
   */
  public static final class Builder {

    private boolean done;
    private Code code;
    private String message;
    private List<String> files = List.of();
    private HistoryEntryInfo entry;
    private HistoryJournalInfo journal;

    private Builder() {}

    /**
     * Creates a builder.
     *
     * @return the builder
     */
    public static Builder create() {
      return new Builder();
    }

    /**
     * Sets whether the step ran.
     *
     * @param done {@code true} when every file was restored
     * @return this builder
     */
    public Builder setDone(boolean done) {
      this.done = done;

      return this;
    }

    /**
     * Sets the refusal code.
     *
     * @param code the code
     * @return this builder
     */
    public Builder setCode(Code code) {
      this.code = code;

      return this;
    }

    /**
     * Sets the refusal reason.
     *
     * @param message the reason
     * @return this builder
     */
    public Builder setMessage(String message) {
      this.message = message;

      return this;
    }

    /**
     * Sets the files the step wrote, or the files that refused it.
     *
     * @param files the absolute paths
     * @return this builder
     */
    public Builder setFiles(List<String> files) {
      this.files = files;

      return this;
    }

    /**
     * Sets the entry the step acted on.
     *
     * @param entry the entry
     * @return this builder
     */
    public Builder setEntry(HistoryEntryInfo entry) {
      this.entry = entry;

      return this;
    }

    /**
     * Sets the journal as it stands after the step.
     *
     * @param journal the journal
     * @return this builder
     */
    public Builder setJournal(HistoryJournalInfo journal) {
      this.journal = journal;

      return this;
    }

    /**
     * Builds the result.
     *
     * @return the result
     */
    public HistoryRestoreResult build() {
      return new HistoryRestoreResult(this);
    }
  }
}
