package com.webforj.plugin.maven.bbj.model;

import java.io.File;
import java.util.Objects;

/**
 * Configures the BBj development run of the watch.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class BBjDevelopmentOptions {

  private File classFolder;

  /**
   * Gets the class folder the BBj machine loads the application classes from.
   *
   * @return the class folder as the BBj machine sees it, or null for the build output directory
   */
  public File getClassFolder() {
    return classFolder;
  }

  /**
   * Sets the class folder the BBj machine loads the application classes from.
   *
   * @param classFolder the class folder as the BBj machine sees it
   *
   * @throws NullPointerException if classFolder is null
   */
  public void setClassFolder(File classFolder) {
    this.classFolder = Objects.requireNonNull(classFolder);
  }
}
