package com.webforj.minify.common;

import java.nio.file.Path;
import java.util.Set;

/**
 * Interface for asset minification implementations.
 *
 * Implementations of this interface are discovered via Java SPI (Service Provider Interface).
 * To register a custom minifier:
 * <ol>
 *   <li>Implement this interface</li>
 *   <li>Create META-INF/services/com.webforj.minify.common.AssetMinifier</li>
 *   <li>List your implementation class in the service file</li>
 *   <li>Add your minifier JAR as a dependency to the project</li>
 * </ol>
 *
 * Implementations must be stateless and thread-safe for parallel processing.
 */
public interface AssetMinifier {

  /**
   * Minifies the content of an asset file.
   *
   * @param content the original file content
   * @param sourceFile the source file path (for error reporting)
   * @return minified content
   * @throws MinificationException if minification fails
   */
  String minify(String content, Path sourceFile) throws MinificationException;

  /**
   * Returns the file extensions this minifier supports.
   *
   * @return set of extensions without dot (e.g., ["css", "scss"])
   */
  Set<String> getSupportedExtensions();
}
