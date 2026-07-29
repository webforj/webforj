package com.webforj.devtools.craftforj.inspector.source.staging.model;

/**
 * One source file held in the staging area, waiting for user approval.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class StagedFile {

  private final String path;
  private final String baseHash;
  private final String content;
  private final boolean isNew;
  private final boolean verified;

  /**
   * Creates a staged file.
   *
   * @param path the absolute path of the target source file
   * @param baseHash the content hash of the file at read time, or {@code null} for a new file
   * @param content the full new file content
   * @param isNew whether the file does not exist on disk yet
   * @param verified whether the content passed full compile validation
   */
  public StagedFile(String path, String baseHash, String content, boolean isNew, boolean verified) {
    this.path = path;
    this.baseHash = baseHash;
    this.content = content;
    this.isNew = isNew;
    this.verified = verified;
  }

  /**
   * Gets the absolute path of the target source file.
   *
   * @return the file path
   */
  public String getPath() {
    return path;
  }

  /**
   * Gets the content hash of the on disk file captured at read time.
   *
   * @return the base hash, or {@code null} for a new file
   */
  public String getBaseHash() {
    return baseHash;
  }

  /**
   * Gets the full new file content.
   *
   * @return the staged content
   */
  public String getContent() {
    return content;
  }

  /**
   * Checks whether the file does not exist on disk yet.
   *
   * @return {@code true} when the file is new
   */
  public boolean isNew() {
    return isNew;
  }

  /**
   * Checks whether the content passed full compile validation.
   *
   * @return {@code true} when compile verified, {@code false} when only parse validated
   */
  public boolean isVerified() {
    return verified;
  }
}
