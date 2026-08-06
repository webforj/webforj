package com.webforj.plugin.gradle.hotswap.jrebel;

import org.gradle.api.file.RegularFileProperty;

/**
 * Configures the JRebel attachment.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public abstract class JrebelOptions {

  /**
   * The JRebel agent on disk, a native library or a jar.
   *
   * @return the agent path property
   */
  public abstract RegularFileProperty getPath();
}
