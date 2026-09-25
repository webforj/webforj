package com.webforj.devtools.craftforj.source.resolver;

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

  private static final List<String> SOURCE_DIRS =
      List.of("src/main/java", "src/main/kotlin", "src", "source", "sources");

  // Keyed by Class, so entries are reclaimed with the classloader that owns them: a hot reload
  // hands out fresh classes and the previous lookups go away with the old loader. The inner map
  // holds entries by extension set and the runtime's optional source filename.
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
    return resolveFile(className, extensions, null);
  }

  /**
   * Resolves a runtime-recorded source filename within the class's package and source roots.
   *
   * @param className the fully qualified class name
   * @param fileName the filename recorded by the runtime, without directories
   * @param extensions the allowed source extensions
   * @return the absolute source path, or null when the recorded file cannot be resolved
   */
  public static String resolve(String className, String fileName, List<String> extensions) {
    if (fileName == null || fileName.isBlank()) {
      return resolve(className, extensions);
    }
    if (fileName.contains("/") || fileName.contains("\\")
        || extensions.stream().noneMatch(fileName::endsWith)) {
      return null;
    }
    return resolveFile(className, extensions, fileName);
  }

  private static String resolveFile(String className, List<String> extensions, String fileName) {
    try {
      Class<?> type = Class.forName(className);
      String key = String.join(",", extensions) + (fileName == null ? "" : ":" + fileName);
      return SOURCE_FILES.get(type)
          .computeIfAbsent(key, ignored -> Optional.ofNullable(search(type, extensions, fileName)))
          .orElse(null);
    } catch (Exception e) {

      return null;
    }
  }

  private static String search(Class<?> type, List<String> extensions, String fileName) {
    try {
      File projectRoot = ProjectRootResolver.resolve(readConfig(), type).toFile();
      List<String> relativePaths;
      if (fileName != null) {
        String packagePath = type.getPackageName().replace('.', File.separatorChar);
        relativePaths =
            List.of(packagePath.isEmpty() ? fileName : packagePath + File.separator + fileName);
      } else {
        Class<?> sourceType = type;
        while (sourceType.getEnclosingClass() != null) {
          sourceType = sourceType.getEnclosingClass();
        }
        String classPath = sourceType.getName().replace('.', File.separatorChar);
        relativePaths = extensions.stream().map(extension -> classPath + extension).toList();
      }

      for (String sourceDir : SOURCE_DIRS) {
        for (String relativePath : relativePaths) {
          File sourceFile = new File(projectRoot, sourceDir + File.separator + relativePath);
          if (sourceFile.isFile()) {
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
