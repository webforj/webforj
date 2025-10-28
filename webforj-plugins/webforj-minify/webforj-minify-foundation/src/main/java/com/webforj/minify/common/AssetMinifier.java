package com.webforj.minify.common;

import java.nio.file.Path;
import java.util.Set;

/**
 * Interface for asset minification implementations.
 *
 * <p>Implementations of this interface are discovered via Java SPI (Service Provider Interface). To
 * register a custom minifier:
 * <ol>
 * <li>Implement this interface</li>
 * <li>Create META-INF/services/com.webforj.minify.common.AssetMinifier</li>
 * <li>List your implementation class in the service file</li>
 * <li>Add your minifier JAR as a dependency to the project</li>
 * </ol>
 *
 * <p><b>Example custom minifier:</b>
 *
 * <pre>{@code
 * public class JsonMinifier implements AssetMinifier {
 *   @Override
 *   public String minify(String content, Path sourceFile) throws MinificationException {
 *     try {
 *       return JsonCompressor.minify(content);
 *     } catch (Exception e) {
 *       throw new MinificationException("Failed to minify " + sourceFile, e);
 *     }
 *   }
 *
 *   @Override
 *   public Set<String> getSupportedExtensions() {
 *     return Set.of("json");
 *   }
 * }
 * }</pre>
 *
 * <p><b>IMPORTANT:</b> Implementations must be stateless and thread-safe for parallel
 * processing. Do not maintain instance state between {@link #minify(String, Path)} calls.
 *
 * @author Kevin Hagel
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

  /**
   * Determines whether the given file should be minified.
   *
   * <p>This method allows minifiers to skip files that are already minified or otherwise should
   * not be processed. The default implementation returns true for all files.
   *
   * @param filePath the file to check
   * @return true if the file should be minified, false to skip it
   */
  default boolean shouldMinify(Path filePath) {
    return true;
  }
}
