package com.webforj.devtools.craftforj.source.model;

import com.webforj.devtools.craftforj.source.staging.SourceHasher;

/**
 * The content of one source file before and after a set of changes.
 *
 * <p>
 * The client diffs the two texts to show what a save would do. Producing the patch here rather than
 * a diff keeps the server free of any diff format, and lets the client render hunks with the line
 * numbers of the real file.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class FilePatch {

  private final String file;
  private final String original;
  private final String patched;
  private final String originalHash;
  private final String patchedHash;

  /**
   * Creates a patch preview for one file.
   *
   * @param file the absolute path of the source file
   * @param original the file content as it is on disk
   * @param patched the file content as it would be written
   */
  public FilePatch(String file, String original, String patched) {
    this.file = file;
    this.original = original;
    this.patched = patched;
    this.originalHash = original == null ? null : SourceHasher.hash(original);
    this.patchedHash = patched == null ? null : SourceHasher.hash(patched);
  }

  /**
   * Gets the absolute path of the source file.
   *
   * @return the file path
   */
  public String getFile() {
    return file;
  }

  /**
   * Gets the file content as it is on disk.
   *
   * @return the original content
   */
  public String getOriginal() {
    return original;
  }

  /**
   * Gets the file content as it would be written.
   *
   * @return the patched content
   */
  public String getPatched() {
    return patched;
  }

  /**
   * Gets the hash of the content as it is on disk.
   *
   * @return the hash of the original content, or {@code null} when there is none
   */
  public String getOriginalHash() {
    return originalHash;
  }

  /**
   * Gets the hash of the content as it would be written.
   *
   * @return the hash of the patched content, or {@code null} when there is none
   */
  public String getPatchedHash() {
    return patchedHash;
  }
}
