package com.webforj.minify.common;

import java.nio.file.Path;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Resolves webforJ resource protocol URLs to filesystem paths.
 *
 * <p>
 * Supports the following protocols:
 * </p>
 * <ul>
 * <li>webserver://path → src/main/resources/static/path</li>
 * <li>ws://path → src/main/resources/static/path</li>
 * <li>context://path → src/main/resources/path</li>
 * </ul>
 *
 * <p>
 * <b>Note:</b> URLs without a protocol (e.g., "app.css") are NOT supported because webforJ passes
 * them through unchanged to the browser, which resolves them as relative URLs. These cannot be
 * reliably mapped to filesystem paths for minification.
 * </p>
 *
 * <p>
 * <b>Security:</b> All resolved paths are validated to prevent directory traversal attacks. Paths
 * containing ".." that escape the resources root will throw {@link SecurityException}.
 * </p>
 *
 * @author Kevin Hagel
 *
 * @see <a href="https://docs.webforj.com/docs/managing-resources/assets-protocols">webforJ Asset
 *      Protocols</a>
 */
public class ResourceResolver {
  private static final Pattern PROTOCOL_PATTERN = Pattern.compile("^([a-z]+)://(.*)$");

  private final Path resourcesRoot;

  /**
   * Creates a resource resolver with a custom resources root.
   *
   * @param resourcesRoot the root directory for resources
   */
  public ResourceResolver(Path resourcesRoot) {
    this.resourcesRoot = resourcesRoot.toAbsolutePath().normalize();
  }

  /**
   * Resolves a webforJ resource URL to a filesystem path.
   *
   * <p>
   * <b>Security:</b> The resolved path is validated to ensure it remains within the resources root
   * directory. Paths attempting to escape via ".." will throw {@link SecurityException}.
   * </p>
   *
   * @param url the resource URL (e.g., "webserver://css/app.css")
   * @return the resolved filesystem path
   * @throws SecurityException if the resolved path escapes the resources root
   * @throws IllegalArgumentException if the protocol is unknown or missing
   */
  public Path resolve(String url) {
    Matcher matcher = PROTOCOL_PATTERN.matcher(url);

    Path resolved;
    if (!matcher.matches()) {
      // No protocol - webforJ passes these through unchanged to the browser
      // They cannot be reliably mapped to filesystem paths for minification
      throw new IllegalArgumentException("URL without protocol cannot be resolved: '" + url
          + "'. Use ws:// or context:// protocol for minifiable resources.");
    } else {
      String protocol = matcher.group(1);
      String path = matcher.group(2);

      resolved = switch (protocol) {
        case "ws", "webserver" -> resourcesRoot.resolve("static").resolve(path);
        case "context" -> resourcesRoot.resolve(path);
        default -> throw new IllegalArgumentException("Unknown protocol: " + protocol);
      };
    }

    // Security validation: ensure resolved path is within resources root
    Path normalizedResolved = resolved.toAbsolutePath().normalize();
    if (!normalizedResolved.startsWith(resourcesRoot)) {
      throw new SecurityException(String.format(
          "Path traversal detected: URL '%s' resolves to '%s' which is outside resources root '%s'",
          url, normalizedResolved, resourcesRoot));
    }

    return normalizedResolved;
  }
}
