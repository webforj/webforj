package com.webforj.devtools.craftforj.inspector.source.resolver;

import com.typesafe.config.Config;
import com.webforj.Environment;
import com.webforj.devtools.craftforj.ProjectRootResolver;
import java.io.File;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Resolves source file paths from class names.
 *
 * <p>
 * Supports multiple source directories and file extensions for JVM languages.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class SourceFileResolver {

  /**
   * Java and Kotlin extensions.
   */
  public static final List<String> ALL_EXTENSIONS = List.of(".java", ".kt");

  /**
   * Java-only extension.
   */
  public static final List<String> JAVA_ONLY = List.of(".java");

  private static final String[] SOURCE_DIRS =
      {"src/main/java", "src/main/kotlin", "src", "source", "sources"};

  // Keyed by Class, so entries are reclaimed with the classloader that owns them: a hot reload
  // hands out fresh classes and the previous lookups go away with the old loader. The inner map
  // holds one entry per extension set, so it stays at two entries at most.
  private static final ClassValue<Map<String, Optional<String>>> SOURCE_FILES = new ClassValue<>() {
    @Override
    protected Map<String, Optional<String>> computeValue(Class<?> type) {
      return new ConcurrentHashMap<>();
    }
  };

  private SourceFileResolver() {}

  /**
   * Checks whether a file path sits under one of the known source directories of a project.
   *
   * @param projectRoot the project root directory
   * @param file the candidate file path
   * @return {@code true} when the file resolves under a known source root
   */
  public static boolean isUnderSourceRoot(Path projectRoot, Path file) {
    Path normalizedFile = file.toAbsolutePath().normalize();
    Path normalizedRoot = projectRoot.toAbsolutePath().normalize();
    for (String sourceDir : SOURCE_DIRS) {
      if (normalizedFile.startsWith(normalizedRoot.resolve(sourceDir).normalize())) {
        return true;
      }
    }

    return false;
  }

  /**
   * Resolves the source file path for a given class name.
   *
   * @param className the fully qualified class name
   * @param extensions the file extensions to search (use ALL_EXTENSIONS or JAVA_ONLY)
   * @return the absolute path to the source file, or null if not found
   */
  public static String resolve(String className, List<String> extensions) {
    try {
      Class<?> type = Class.forName(className);
      return SOURCE_FILES.get(type).computeIfAbsent(String.join(",", extensions),
          ignored -> Optional.ofNullable(search(type, className, extensions))).orElse(null);
    } catch (Exception e) {

      return null;
    }
  }

  private static String search(Class<?> type, String className, List<String> extensions) {
    try {
      File projectRoot = ProjectRootResolver.resolve(readConfig(), type).toFile();
      String classPath = className.replace('.', File.separatorChar);

      for (String sourceDir : SOURCE_DIRS) {
        for (String ext : extensions) {
          File sourceFile = new File(projectRoot, sourceDir + File.separator + classPath + ext);
          if (sourceFile.exists()) {
            return sourceFile.getAbsolutePath();
          }
        }
      }

      return null;
    } catch (Exception e) {

      return null;
    }
  }

  private static Config readConfig() {
    Environment env = Environment.getCurrent();
    return env == null ? null : env.getConfig();
  }
}
