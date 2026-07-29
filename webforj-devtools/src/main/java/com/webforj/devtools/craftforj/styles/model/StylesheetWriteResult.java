package com.webforj.devtools.craftforj.styles.model;

/**
 * Result of a compare and swap stylesheet write.
 *
 * <p>
 * On success {@code applied} is {@code true} and {@code version} identifies the written content. On
 * a version conflict {@code conflict} is {@code true} and {@code content} plus {@code version}
 * carry the current file state so the caller can re-apply its change and retry.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class StylesheetWriteResult {

  private String path;
  private boolean applied;
  private boolean conflict;
  private String content;
  private String version;

  /**
   * Gets the stylesheet path.
   *
   * @return the path
   */
  public String getPath() {
    return path;
  }

  /**
   * Sets the stylesheet path.
   *
   * @param path the path
   */
  public void setPath(String path) {
    this.path = path;
  }

  /**
   * Indicates whether the content was written.
   *
   * @return {@code true} when the file was written
   */
  public boolean isApplied() {
    return applied;
  }

  /**
   * Sets whether the content was written.
   *
   * @param applied the applied flag
   */
  public void setApplied(boolean applied) {
    this.applied = applied;
  }

  /**
   * Indicates whether the write was rejected because the file changed since the base version.
   *
   * @return {@code true} on a version conflict
   */
  public boolean isConflict() {
    return conflict;
  }

  /**
   * Sets whether the write was rejected because the file changed.
   *
   * @param conflict the conflict flag
   */
  public void setConflict(boolean conflict) {
    this.conflict = conflict;
  }

  /**
   * Gets the current file content, only set on a conflict.
   *
   * @return the content
   */
  public String getContent() {
    return content;
  }

  /**
   * Sets the current file content.
   *
   * @param content the content
   */
  public void setContent(String content) {
    this.content = content;
  }

  /**
   * Gets the version of the file content after the call.
   *
   * @return the version
   */
  public String getVersion() {
    return version;
  }

  /**
   * Sets the version of the file content after the call.
   *
   * @param version the version
   */
  public void setVersion(String version) {
    this.version = version;
  }
}
