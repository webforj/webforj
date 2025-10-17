package com.webforj.minify.common;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Resolves webforJ resource protocol URLs to filesystem paths.
 *
 * Supports the following protocols:
 * <ul>
 *   <li>webserver://path → src/main/resources/static/path</li>
 *   <li>ws://path → src/main/resources/static/path</li>
 *   <li>context://path → src/main/resources/path</li>
 *   <li>No protocol → static/path</li>
 * </ul>
 *
 * @see <a href="https://docs.webforj.com/docs/managing-resources/assets-protocols">webforJ Asset Protocols</a>
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
    this.resourcesRoot = resourcesRoot;
  }

  /**
   * Resolves a webforJ resource URL to a filesystem path.
   *
   * @param url the resource URL (e.g., "webserver://css/app.css")
   * @return the resolved filesystem path
   */
  public Path resolve(String url) {
    Matcher matcher = PROTOCOL_PATTERN.matcher(url);

    if (!matcher.matches()) {
      // No protocol - treat as static path
      return resourcesRoot.resolve("static").resolve(url);
    }

    String protocol = matcher.group(1);
    String path = matcher.group(2);

    switch (protocol) {
      case "webserver":
      case "ws":
        return resourcesRoot.resolve("static").resolve(path);
      case "context":
        return resourcesRoot.resolve(path);
      default:
        // Unknown protocol - return as-is
        return Paths.get(url);
    }
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
