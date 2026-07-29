package com.webforj.devtools.craftforj.styles.model;

/**
 * Result of a stylesheet change (preview or apply).
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class StylesheetResult {

  private String path;
  private String version;
  private boolean conflict;
  private boolean applied;
  private String content;
  private String error;

  /**
   * Gets the absolute stylesheet path.
   *
   * @return the absolute path
   */
  public String getPath() {
    return path;
  }

  /**
   * Sets the absolute stylesheet path.
   *
   * @param path the absolute path
   */
  public void setPath(String path) {
    this.path = path;
  }

  /**
   * Returns whether the change was written to disk.
   *
   * @return {@code true} when written, {@code false} for previews and failures
   */
  public boolean isApplied() {
    return applied;
  }

  /**
   * Sets whether the change was written to disk.
   *
   * @param applied {@code true} when written
   */
  public void setApplied(boolean applied) {
    this.applied = applied;
  }

  /**
   * Gets the resulting stylesheet content.
   *
   * @return the content after the edits, or {@code null} on failure
   */
  public String getContent() {
    return content;
  }

  /**
   * Sets the resulting stylesheet content.
   *
   * @param content the content after the edits
   */
  public void setContent(String content) {
    this.content = content;
  }

  /**
   * Gets the error message.
   *
   * @return the error, or {@code null} when successful
   */
  public String getError() {
    return error;
  }

  /**
   * Sets the error message.
   *
   * @param error the error message
   */
  public void setError(String error) {
    this.error = error;
  }

  /**
   * Gets the content version after the write, used as the base for the next one.
   *
   * @return the version
   */
  public String getVersion() {
    return version;
  }

  /**
   * Sets the content version after the write.
   *
   * @param version the version
   */
  public void setVersion(String version) {
    this.version = version;
  }

  /**
   * Returns whether the file moved under the caller since it read the base version.
   *
   * @return {@code true} on a conflict, when nothing was written
   */
  public boolean isConflict() {
    return conflict;
  }

  /**
   * Sets whether the file moved under the caller.
   *
   * @param conflict {@code true} when nothing was written
   */
  public void setConflict(boolean conflict) {
    this.conflict = conflict;
  }
}
