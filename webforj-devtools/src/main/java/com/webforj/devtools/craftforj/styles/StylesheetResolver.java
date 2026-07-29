package com.webforj.devtools.craftforj.styles;

import com.webforj.devtools.craftforj.action.CraftforjActionException;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * Resolves the application stylesheet the craftforJ AI assistant reads and writes.
 *
 * <p>
 * When the developer configured a stylesheet path in the craftforJ settings, that path wins.
 * Otherwise the resolver defaults to the frontend bundler layout at
 * {@code src/main/frontend/app.css}, the convention for webforJ 26 projects. When that file is
 * missing but the static-resources convention {@code src/main/resources/static/app.css} exists on
 * disk, the existing one is used, so projects that keep their stylesheet there still work.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class StylesheetResolver {

  /**
   * Conventional stylesheet for the frontend bundler layout, the default for webforJ 26 projects.
   */
  public static final String FRONTEND_STYLESHEET = "src/main/frontend/app.css";

  /**
   * Conventional stylesheet served from static resources, used as a fallback.
   */
  public static final String STATIC_STYLESHEET = "src/main/resources/static/app.css";

  private final Path projectRoot;

  /**
   * Creates a resolver for the given project.
   *
   * @param projectRoot the project root directory
   */
  public StylesheetResolver(Path projectRoot) {
    this.projectRoot = projectRoot;
  }

  /**
   * Resolves the stylesheet path.
   *
   * <p>
   * Configured paths are confined to the project root: absolute paths are rejected, the resolved
   * path must stay inside the project root, and it must point to a {@code .css} file.
   * </p>
   *
   * @param configuredPath the developer-configured path relative to the project root, or
   *        {@code null} to use the layout default
   * @return the resolved absolute path
   * @throws CraftforjActionException if the configured path is absolute, escapes the project root,
   *         or does not end in {@code .css}
   */
  public Path resolve(String configuredPath) {
    if (configuredPath != null && !configuredPath.isBlank()) {
      Path configured = Path.of(configuredPath);
      if (configured.isAbsolute()) {
        throw new CraftforjActionException(
            "Stylesheet path must be relative to the project root: " + configuredPath);
      }

      Path root = projectRoot.normalize();
      Path resolved = root.resolve(configured).normalize();
      if (!resolved.startsWith(root)) {
        throw new CraftforjActionException(
            "Stylesheet path escapes the project root: " + configuredPath);
      }

      if (!resolved.getFileName().toString().endsWith(".css")) {
        throw new CraftforjActionException(
            "Stylesheet path must point to a .css file: " + configuredPath);
      }

      return resolved;
    }

    return defaultPath();
  }

  /**
   * Gets the default stylesheet path.
   *
   * <p>
   * Prefers the frontend convention; falls back to the static-resources convention when only that
   * one exists on disk.
   * </p>
   *
   * @return the default absolute path
   */
  public Path defaultPath() {
    Path frontend = projectRoot.resolve(FRONTEND_STYLESHEET);
    Path staticResource = projectRoot.resolve(STATIC_STYLESHEET);

    if (!Files.isRegularFile(frontend) && Files.isRegularFile(staticResource)) {
      return staticResource.normalize();
    }

    return frontend.normalize();
  }
}
