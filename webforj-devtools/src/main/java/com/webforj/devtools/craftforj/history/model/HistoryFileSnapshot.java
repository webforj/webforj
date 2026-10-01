package com.webforj.devtools.craftforj.history.model;

/**
 * One file a history entry changed, with the hashes of its content before and after.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@SuppressWarnings("java:S6206")
public final class HistoryFileSnapshot {

  private final String path;
  private final String before;
  private final String after;

  /**
   * Creates the snapshot of one file.
   *
   * @param path the absolute path of the file
   * @param before the hash of the content before the write, or {@code null} when it did not exist
   * @param after the hash of the content after the write, or {@code null} when it was deleted
   */
  public HistoryFileSnapshot(String path, String before, String after) {
    this.path = path;
    this.before = before;
    this.after = after;
  }

  /**
   * Gets the absolute path of the file.
   *
   * @return the path
   */
  public String getPath() {
    return path;
  }

  /**
   * Gets the hash of the content before the write.
   *
   * @return the hash, or {@code null} when the file did not exist
   */
  public String getBefore() {
    return before;
  }

  /**
   * Gets the hash of the content after the write.
   *
   * @return the hash, or {@code null} when the write deleted the file
   */
  public String getAfter() {
    return after;
  }
}
