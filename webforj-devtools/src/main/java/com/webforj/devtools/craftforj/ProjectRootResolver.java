package com.webforj.devtools.craftforj;

import com.typesafe.config.Config;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * Resolves the root directory of the running project.
 *
 * <p>
 * The configured {@value #KEY_PROJECT_ROOT} always wins. Without it, the root is derived from the
 * code source of an anchor class by walking up from the class location until a directory holds a
 * build file, which covers exploded Maven and Gradle runs alike. A jar deployment, such as an app
 * installed into BBjServices, carries no relation to the project on disk, so derivation fails there
 * and the JVM working directory is the last resort.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class ProjectRootResolver {

  /**
   * The configuration key naming the project root directory on disk.
   */
  public static final String KEY_PROJECT_ROOT = "webforj.devtools.craftforj.project-root";

  private static final System.Logger LOGGER = System.getLogger(ProjectRootResolver.class.getName());
  private static final String[] BUILD_MARKERS =
      {"pom.xml", "build.gradle", "build.gradle.kts", "settings.gradle", "settings.gradle.kts"};

  private ProjectRootResolver() {}

  /**
   * Resolves the project root for the given anchor class.
   *
   * @param config the webforJ configuration, may be {@code null}
   * @param anchor the class whose code source seeds the derivation
   * @return the configured root, the derived root, or the JVM working directory
   */
  public static Path resolve(Config config, Class<?> anchor) {
    Path configured = readConfiguredRoot(config);
    if (configured != null) {
      return configured;
    }

    Path derived = deriveFromCodeSource(anchor);
    if (derived != null) {
      return derived;
    }

    return Path.of(System.getProperty("user.dir"));
  }

  /**
   * Reads the configured project root.
   *
   * @param config the webforJ configuration, may be {@code null}
   * @return the configured directory, or {@code null} when absent or unusable
   */
  static Path readConfiguredRoot(Config config) {
    if (config == null || !config.hasPath(KEY_PROJECT_ROOT) || config.getIsNull(KEY_PROJECT_ROOT)) {
      return null;
    }

    String value = config.getString(KEY_PROJECT_ROOT).trim();
    if (value.isEmpty()) {
      return null;
    }

    Path root = Path.of(value).toAbsolutePath().normalize();
    if (!Files.isDirectory(root)) {
      LOGGER.log(System.Logger.Level.WARNING,
          "The configured {0} is not a directory and is ignored, {1}", KEY_PROJECT_ROOT, root);

      return null;
    }

    return root;
  }

  /**
   * Derives the project root from the code source of the anchor class.
   *
   * <p>
   * Walks up from the class location until a directory holds a build file.
   * </p>
   *
   * @param anchor the class whose code source seeds the derivation
   * @return the derived root, or {@code null} when the location gives nothing away
   */
  static Path deriveFromCodeSource(Class<?> anchor) {
    try {
      var location = anchor.getProtectionDomain().getCodeSource().getLocation();
      Path start = Path.of(location.toURI()).toAbsolutePath().normalize();
      Path current = Files.isDirectory(start) ? start : start.getParent();

      while (current != null) {
        if (isProjectRoot(current)) {
          return current;
        }

        current = current.getParent();
      }
    } catch (Exception e) {
      LOGGER.log(System.Logger.Level.DEBUG, "Could not derive the project root", e);
    }

    return null;
  }

  private static boolean isProjectRoot(Path directory) {
    for (String marker : BUILD_MARKERS) {
      if (Files.exists(directory.resolve(marker))) {
        return true;
      }
    }

    return false;
  }
}
