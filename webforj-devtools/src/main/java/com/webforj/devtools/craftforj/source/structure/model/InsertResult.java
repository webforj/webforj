package com.webforj.devtools.craftforj.source.structure.model;

import com.webforj.devtools.craftforj.source.model.FilePatch;
import com.webforj.devtools.craftforj.source.model.SourceLocation;
import java.util.List;

/**
 * The files an insert changed and where the new component is created in them.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class InsertResult { // NOSONAR

  private final List<FilePatch> files;
  private final SourceLocation location;

  /**
   * Creates a result.
   *
   * @param files the patches of the files that change
   * @param location the file, line and variable of the new creation, or {@code null}
   */
  public InsertResult(List<FilePatch> files, SourceLocation location) {
    this.files = files;
    this.location = location;
  }

  /**
   * Gets the patches of the files that change.
   *
   * @return the patches
   */
  public List<FilePatch> getFiles() {
    return files;
  }

  /**
   * Gets where the new component is created, in the patched content.
   *
   * @return the file, line and variable of the new creation, {@code null} when the file did not
   *         change
   */
  public SourceLocation getLocation() {
    return location;
  }
}
