package com.webforj.plugin.maven.hotswap.jrebel;

import java.io.File;

/**
 * Configures the JRebel attachment.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class JrebelOptions {

  private File path;

  /**
   * The JRebel agent on disk, a native library or a jar.
   *
   * @return the agent path
   */
  public File getPath() {
    return path;
  }

  /**
   * Sets the JRebel agent path.
   *
   * @param path the agent path
   * @return this options instance
   */
  public JrebelOptions setPath(File path) {
    this.path = path;
    return this;
  }
}
