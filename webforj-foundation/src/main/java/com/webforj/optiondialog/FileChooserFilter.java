package com.webforj.optiondialog;

/**
 * Represents a file filter for use in file choosers or file uploads. It holds a description and a
 * corresponding file mask.
 *
 * @author Hyyan Abo Fakher
 * @since 24.02
 */
public class FileChooserFilter {
  private String description = "";
  private String filter = "";

  /**
   * Constructs a FileFilter with the specified description and mask.
   *
   * @param description the description of the file pattern (e.g., "Text Files (*.txt)")
   * @param filter the mask for the file pattern (e.g., "*.txt")
   */
  public FileChooserFilter(String description, String filter) {
    this.description = description == null ? "" : description;
    this.filter = filter == null ? "" : filter;
  }

  /**
   * Returns the description of the file pattern.
   *
   * @return the description of the file pattern
   */
  public String getDescription() {
    return description;
  }

  /**
   * Returns the mask of the file filter.
   *
   * @return the mask of the file filter
   */
  public String getPattern() {
    return filter;
  }
}
