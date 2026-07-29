package com.webforj.devtools.craftforj.inspector.source.model;

/**
 * The content of one source file before and after a set of changes, without writing anything.
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
}
