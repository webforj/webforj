package com.webforj.minify.common;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Resolves webforJ resource protocol URLs to filesystem paths.
 *
 * <p>
 * Supports the following protocols:
 * <ul>
 * <li>webserver://path → src/main/resources/static/path</li>
 * <li>ws://path → src/main/resources/static/path</li>
 * <li>context://path → src/main/resources/path</li>
 * <li>No protocol → static/path</li>
 * </ul>
 *
 * <p>
 * <b>Security:</b> All resolved paths are validated to prevent directory traversal attacks. Paths
 * containing ".." that escape the resources root will throw {@link SecurityException}.
 *
 * @see <a href="https://docs.webforj.com/docs/managing-resources/assets-protocols">webforJ Asset
 *      Protocols</a>
 */
public class ResourceResolver {
  private static final Pattern PROTOCOL_PATTERN = Pattern.compile("^([a-z]+)://(.*)$");

  private final Path resourcesRoot;

  /**
   * Creates a resource resolver with the default resources root (src/main/resources).
   */
  public ResourceResolver() {
    this(Paths.get("src", "main", "resources"));
  }

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
   *
   * @param url the resource URL (e.g., "webserver://css/app.css")
   * @return the resolved filesystem path
   * @throws SecurityException if the resolved path escapes the resources root
   * @throws IllegalArgumentException if the protocol is unknown
   */
  public Path resolve(String url) {
    Matcher matcher = PROTOCOL_PATTERN.matcher(url);

    Path resolved;
    if (!matcher.matches()) {
      // No protocol - treat as static path
      resolved = resourcesRoot.resolve("static").resolve(url);
    } else {
      String protocol = matcher.group(1);
      String path = matcher.group(2);

      resolved = switch (protocol) {
        case "webserver", "ws" -> resourcesRoot.resolve("static").resolve(path);
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

  /**
   * Gets the resources root directory.
   *
   * @return the resources root path
   */
  public Path getResourcesRoot() {
    return resourcesRoot;
  }
}
