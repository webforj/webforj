package com.webforj.minify.gradle;

import org.gradle.api.provider.Property;

/**
 * Configuration extension for the webforJ minify plugin.
 *
 * <p>Usage in build.gradle:
 * <pre>
 * webforjMinify {
 *   skip = false
 * }
 * </pre>
 */
public interface MinifyExtension {

  /**
   * Whether to skip minification.
   *
   * @return the skip property
   */
  Property<Boolean> getSkip();
}
