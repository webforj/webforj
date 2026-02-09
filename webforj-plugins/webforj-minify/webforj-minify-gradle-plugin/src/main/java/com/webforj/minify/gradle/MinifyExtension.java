package com.webforj.minify.gradle;

import java.util.Map;
import org.gradle.api.provider.MapProperty;
import org.gradle.api.provider.Property;

/**
 * Configuration extension for the webforJ minify plugin.
 *
 * <p>
 * Usage in build.gradle.kts:
 * </p>
 *
 * <pre>
 * webforjMinify {
 *   skip.set(false)
 *   minifierConfigurations.put("closureJs", mapOf(
 *     "compilationLevel" to "SIMPLE_OPTIMIZATIONS",
 *     "languageIn" to "ECMASCRIPT_NEXT",
 *     "languageOut" to "ECMASCRIPT5"
 *   ))
 * }
 * </pre>
 *
 * @author Kevin Hagel
 */
public interface MinifyExtension {

  /**
   * Whether to skip minification.
   *
   * @return the skip property
   */
  Property<Boolean> getSkip();

  /**
   * Minifier-specific configuration options.
   *
   * <p>
   * Each entry maps a minifier configuration key (e.g., "closureJs") to a map of key-value option
   * pairs. The configuration is passed to {@code MinifierRegistry.configureMinifiers()} which
   * delegates to each minifier's {@code configure()} method.
   * </p>
   *
   * <p>
   * This mirrors the Maven plugin's {@code <minifierConfigurations>} parameter to ensure
   * build-system parity.
   * </p>
   *
   * @return the minifier configurations map property
   */
  MapProperty<String, Map<String, String>> getMinifierConfigurations();
}
