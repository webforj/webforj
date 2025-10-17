package com.webforj.minify.gradle;

import org.gradle.api.provider.Property;

/**
 * Extension for configuring the webforJ minify plugin.
 *
 * Usage in build.gradle:
 * <pre>
 * webforjMinify {
 *   enabled = true
 * }
 * </pre>
 */
public abstract class MinifyExtension {

  /**
   * Whether minification is enabled.
   *
   * @return property for enabled flag
   */
  public abstract Property<Boolean> getEnabled();

  public MinifyExtension() {
    // Default to enabled
    getEnabled().convention(true);
  }
}
