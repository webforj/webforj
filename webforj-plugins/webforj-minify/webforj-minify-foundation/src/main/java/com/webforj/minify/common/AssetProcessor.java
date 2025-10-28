package com.webforj.minify.common;

import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonSyntaxException;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Stream;

/**
 * Core asset processing logic shared between Maven and Gradle plugins.
 *
 * <p>This class extracts the common minification workflow to eliminate code duplication.
 */
public class AssetProcessor {

  private final BuildLogger logger;
  private final Gson gson = new Gson();
  private final MinifierRegistry registry = new MinifierRegistry();
  private final Set<Path> processedFiles = new HashSet<>();

  /**
   * Creates a new AssetProcessor with the specified logger.
   *
   * @param logger the logger to use for output
   */
  public AssetProcessor(BuildLogger logger) {
    this.logger = logger;
  }

  /**
   * Gets the minifier registry.
   *
   * @return the minifier registry
   */
  public MinifierRegistry getRegistry() {
    return registry;
  }

  /**
   * Gets the count of processed files.
   *
   * @return the number of files processed
   */
  public int getProcessedFileCount() {
    return processedFiles.size();
  }

  /**
   * Processes a manifest file containing asset URLs.
   *
   * @param manifestPath path to the manifest file
   * @param resolver resource resolver for converting URLs to file paths
   * @throws JsonSyntaxException if the manifest is malformed
   */
  public void processManifest(Path manifestPath, ResourceResolver resolver) {
    try {
      String content = Files.readString(manifestPath, StandardCharsets.UTF_8);
      JsonObject manifest = gson.fromJson(content, JsonObject.class);

      // Check for "assets" (new format) or "resources" (legacy format)
      JsonArray assets = null;
      if (manifest.has("assets")) {
        assets = manifest.getAsJsonArray("assets");
      } else if (manifest.has("resources")) {
        assets = manifest.getAsJsonArray("resources");
      }

      if (assets == null || assets.size() == 0) {
        logger.warn("Manifest file contains no assets");
        return;
      }

      logger.info("Found " + assets.size() + " asset(s) in manifest");

      // Collect all file paths first
      Set<Path> filesToProcess = new HashSet<>();
      collectFilesToProcess(assets, resolver, filesToProcess);

      // Process files (use parallel streams for >10 files)
      if (filesToProcess.size() > 10) {
        logger.info("Using parallel processing for " + filesToProcess.size() + " files");
        filesToProcess.parallelStream().forEach(this::processFile);
      } else {
        filesToProcess.forEach(this::processFile);
      }

    } catch (IOException e) {
      logger.error("Failed to read manifest file: " + e.getMessage(), e);
    } catch (JsonSyntaxException e) {
      logger.error("Malformed manifest file: " + e.getMessage(), e);
      throw e;
    }
  }

  private void collectFilesToProcess(JsonArray assets, ResourceResolver resolver,
      Set<Path> filesToProcess) {
    for (JsonElement element : assets) {
      JsonObject resource = element.getAsJsonObject();
      String url = resource.get("url").getAsString();

      try {
        Path filePath = resolver.resolve(url);

        // Validate file existence - warn and skip if file doesn't exist
        if (!Files.exists(filePath)) {
          logger.warn("File not found: " + filePath + " (referenced as '" + url + "')");
        } else {
          filesToProcess.add(filePath);
        }
      } catch (SecurityException e) {
        logger.warn("Security violation for URL '" + url + "': " + e.getMessage());
      } catch (IllegalArgumentException e) {
        logger.warn("Failed to resolve URL '" + url + "': " + e.getMessage());
      }
    }
  }

  /**
   * Processes a configuration file containing glob patterns for additional assets.
   *
   * @param configPath path to the configuration file
   * @param resourcesRoot root directory for resolving relative paths
   */
  public void processConfigFile(Path configPath, Path resourcesRoot) {
    try {
      // Parse patterns into inclusion and exclusion lists
      Set<String> inclusionPatterns = new HashSet<>();
      Set<String> exclusionPatterns = new HashSet<>();

      Files.lines(configPath, StandardCharsets.UTF_8).map(String::trim)
          .filter(line -> !line.isEmpty() && !line.startsWith("#")).forEach(pattern -> {
            if (pattern.startsWith("!")) {
              // Exclusion pattern - remove ! prefix and add to exclusions
              exclusionPatterns.add(pattern.substring(1));
              logger.debug("Exclusion pattern: " + pattern.substring(1));
            } else {
              // Inclusion pattern
              inclusionPatterns.add(pattern);
              logger.debug("Inclusion pattern: " + pattern);
            }
          });

      // Collect all files matching inclusion patterns
      Set<Path> filesToProcess = new HashSet<>();
      for (String pattern : inclusionPatterns) {
        collectFilesMatchingPattern(resourcesRoot, pattern, filesToProcess);
      }

      // Filter out files matching any exclusion pattern
      if (!exclusionPatterns.isEmpty()) {
        filesToProcess.removeIf(file -> exclusionPatterns.stream()
            .anyMatch(pattern -> matchesGlob(resourcesRoot, file, pattern)));
      }

      // Process remaining files
      filesToProcess.forEach(this::processFile);

    } catch (IOException e) {
      logger.warn("Failed to read config file: " + e.getMessage());
    }
  }

  private void collectFilesMatchingPattern(Path root, String pattern, Set<Path> files) {
    try (Stream<Path> paths = Files.walk(root)) {
      paths.filter(Files::isRegularFile).filter(p -> matchesGlob(root, p, pattern))
          .forEach(files::add);
    } catch (IOException e) {
      logger.warn("Error processing glob pattern " + pattern + ": " + e.getMessage());
    }
  }

  private boolean matchesGlob(Path root, Path file, String pattern) {
    String relativePath = root.relativize(file).toString().replace('\\', '/');
    String regex = convertGlobToRegex(pattern);
    return relativePath.matches(regex);
  }

  /**
   * Converts a glob pattern to a regular expression.
   *
   * <p>Supports the following glob patterns:
   *
   * <ul>
   * <li>** - matches zero or more path segments (directories)
   * <li>* - matches zero or more characters within a single path segment (not /)
   * <li>? - matches exactly one character (not /)
   * <li>All other characters are treated literally
   * </ul>
   *
   * @param glob the glob pattern to convert
   * @return the equivalent regular expression
   */
  private String convertGlobToRegex(String glob) {
    StringBuilder regex = new StringBuilder();
    int i = 0;

    while (i < glob.length()) {
      char c = glob.charAt(i);
      i += processGlobCharacter(c, glob, i, regex);
    }

    return regex.toString();
  }

  private int processGlobCharacter(char c, String glob, int index, StringBuilder regex) {
    if (c == '*') {
      return handleAsterisk(glob, index, regex);
    }
    if (c == '?') {
      regex.append("[^/]");
      return 1;
    }
    if ("\\[]{}()+|^$.".indexOf(c) >= 0) {
      regex.append('\\').append(c);
      return 1;
    }
    regex.append(c);
    return 1;
  }

  private int handleAsterisk(String glob, int index, StringBuilder regex) {
    // Check for **
    if (index + 1 < glob.length() && glob.charAt(index + 1) == '*') {
      regex.append(".*");
      int newIndex = index + 2;
      // Skip trailing slash after **
      if (newIndex < glob.length() && glob.charAt(newIndex) == '/') {
        return 3;
      }
      return 2;
    }
    // Single * matches zero or more characters except /
    regex.append("[^/]*");
    return 1;
  }

  private synchronized void processFile(Path filePath) {
    // Skip if already processed (synchronized for thread safety)
    if (processedFiles.contains(filePath)) {
      return;
    }

    // Skip if doesn't exist
    if (!Files.exists(filePath)) {
      logger.warn("File not found: " + filePath);
      return;
    }

    // Get file extension
    String fileName = filePath.getFileName().toString();
    int lastDot = fileName.lastIndexOf('.');
    if (lastDot < 0) {
      return; // No extension
    }

    String extension = fileName.substring(lastDot + 1);

    // Find minifier for this extension
    AssetMinifier minifier = registry.getMinifier(extension).orElse(null);
    if (minifier == null) {
      logger.debug("No minifier found for extension ." + extension + ": " + fileName);
      return;
    }

    // Ask minifier if this file should be processed
    if (!minifier.shouldMinify(filePath)) {
      return;
    }

    // Minify the file
    try {
      long fileStartTime = System.currentTimeMillis();
      String content = Files.readString(filePath, StandardCharsets.UTF_8);
      long originalSize = content.length();

      String minified = minifier.minify(content, filePath);
      long minifiedSize = minified.length();

      // Only write if content changed
      if (!content.equals(minified)) {
        Files.writeString(filePath, minified, StandardCharsets.UTF_8);
        long duration = System.currentTimeMillis() - fileStartTime;

        double reductionPercent = 100.0 * (originalSize - minifiedSize) / originalSize;
        logger.info(String.format("Minified %s: %d → %d bytes (%.1f%% reduction) in %d ms",
            fileName, originalSize, minifiedSize, reductionPercent, duration));
      } else {
        logger.debug("No changes after minification: " + fileName);
      }

      processedFiles.add(filePath);

    } catch (IOException e) {
      logger.warn("Error reading file " + filePath + ": " + e.getMessage());
    } catch (MinificationException e) {
      logger.warn("Error minifying file " + filePath + ": " + e.getMessage());
    } catch (Exception e) {
      logger.warn("Unexpected error processing " + filePath + ": " + e.getMessage());
    }
  }
}
