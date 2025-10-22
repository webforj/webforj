package com.webforj.minify.gradle;

import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.webforj.minify.common.AssetMinifier;
import com.webforj.minify.common.MinificationException;
import com.webforj.minify.common.MinifierRegistry;
import com.webforj.minify.common.ResourceResolver;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Stream;
import org.gradle.api.DefaultTask;
import org.gradle.api.GradleException;
import org.gradle.api.file.DirectoryProperty;
import org.gradle.api.provider.Property;
import org.gradle.api.tasks.Input;
import org.gradle.api.tasks.InputDirectory;
import org.gradle.api.tasks.Optional;
import org.gradle.api.tasks.TaskAction;

/**
 * Gradle task that minifies webforJ assets during the build process.
 */
public abstract class MinifyTask extends DefaultTask {

  private final Gson gson = new Gson();
  private final MinifierRegistry registry = new MinifierRegistry();
  private final Set<Path> processedFiles = new HashSet<>();

  protected MinifyTask() {
    setGroup("webforJ");
    setDescription("Minifies webforJ assets");
  }

  @InputDirectory
  public abstract DirectoryProperty getOutputDirectory();

  @InputDirectory
  public abstract DirectoryProperty getResourcesDirectory();

  @Input
  @Optional
  public abstract Property<Boolean> getSkip();

  /**
   * Executes the minification task.
   */
  @TaskAction
  public void minify() {
    if (Boolean.TRUE.equals(getSkip().getOrElse(false))) {
      getLogger().info("Minification skipped (skip=true)");
      return;
    }

    final long startTime = System.currentTimeMillis();
    getLogger().info("Starting webforJ asset minification...");

    // Load minifiers via SPI
    registry.loadMinifiers(getClass().getClassLoader());

    if (registry.getMinifierCount() == 0) {
      getLogger().warn("No minifiers registered via SPI. Skipping minification.");
      getLogger().warn("Ensure ph-css and/or closure-compiler are on the classpath.");
      return;
    }

    getLogger().info("Discovered {} minifier implementation(s) via SPI",
        registry.getMinifierCount());

    // Process manifest file
    File outputDir = getOutputDirectory().get().getAsFile();
    Path manifestPath =
        Paths.get(outputDir.getAbsolutePath(), "META-INF", "webforj-resources.json");

    if (Files.exists(manifestPath)) {
      getLogger().info("Processing manifest: {}", manifestPath);
      processManifest(manifestPath);
    } else {
      getLogger().debug("No manifest file found at {}", manifestPath);
    }

    // Process additional configuration file
    File resourcesDir = getResourcesDirectory().get().getAsFile();
    Path configPath = Paths.get(resourcesDir.getAbsolutePath(), "META-INF", "webforj-minify.txt");

    if (Files.exists(configPath)) {
      getLogger().info("Processing configuration file: {}", configPath);
      processConfigFile(configPath, resourcesDir.toPath());
    }

    long duration = System.currentTimeMillis() - startTime;
    getLogger().info("Minification complete. Processed {} file(s) in {} ms",
        processedFiles.size(), duration);
  }

  private void processManifest(Path manifestPath) {
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
        getLogger().warn("Manifest file contains no assets");
        return;
      }

      getLogger().info("Found {} asset(s) in manifest", assets.size());

      File resourcesDir = getResourcesDirectory().get().getAsFile();
      ResourceResolver resolver = new ResourceResolver(resourcesDir.toPath());

      // Collect all file paths first
      Set<Path> filesToProcess = new HashSet<>();
      collectFilesToProcess(assets, resolver, filesToProcess);

      // Process files (use parallel streams for >10 files)
      if (filesToProcess.size() > 10) {
        getLogger().info("Using parallel processing for {} files", filesToProcess.size());
        filesToProcess.parallelStream().forEach(this::processFile);
      } else {
        filesToProcess.forEach(this::processFile);
      }

    } catch (IOException e) {
      getLogger().error("Failed to read manifest file: {}", e.getMessage(), e);
    } catch (Exception e) {
      getLogger().error("Malformed manifest file: {}", e.getMessage(), e);
      throw new GradleException(
          "Malformed manifest file - check META-INF/webforj-resources.json", e);
    }
  }

  private void collectFilesToProcess(JsonArray assets, ResourceResolver resolver,
      Set<Path> filesToProcess) {
    for (JsonElement element : assets) {
      JsonObject resource = element.getAsJsonObject();
      String url = resource.get("url").getAsString();

      try {
        Path filePath = resolver.resolve(url);
        filesToProcess.add(filePath);
      } catch (SecurityException e) {
        getLogger().warn("Security violation for URL '{}': {}", url, e.getMessage());
      } catch (Exception e) {
        getLogger().warn("Failed to resolve URL '{}': {}", url, e.getMessage());
      }
    }
  }

  private void processConfigFile(Path configPath, Path resourcesRoot) {
    try {
      // Parse patterns into inclusion and exclusion lists
      Set<String> inclusionPatterns = new HashSet<>();
      Set<String> exclusionPatterns = new HashSet<>();

      Files.lines(configPath, StandardCharsets.UTF_8).map(String::trim)
          .filter(line -> !line.isEmpty() && !line.startsWith("#")).forEach(pattern -> {
            if (pattern.startsWith("!")) {
              // Exclusion pattern - remove ! prefix and add to exclusions
              exclusionPatterns.add(pattern.substring(1));
              getLogger().debug("Exclusion pattern: {}", pattern.substring(1));
            } else {
              // Inclusion pattern
              inclusionPatterns.add(pattern);
              getLogger().debug("Inclusion pattern: {}", pattern);
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
      getLogger().warn("Failed to read config file: {}", e.getMessage());
    }
  }

  private void collectFilesMatchingPattern(Path root, String pattern, Set<Path> files) {
    try (Stream<Path> paths = Files.walk(root)) {
      paths.filter(Files::isRegularFile).filter(p -> matchesGlob(root, p, pattern))
          .forEach(files::add);
    } catch (IOException e) {
      getLogger().warn("Error processing glob pattern {}: {}", pattern, e.getMessage());
    }
  }

  private boolean matchesGlob(Path root, Path file, String pattern) {
    String relativePath = root.relativize(file).toString().replace('\\', '/');
    return relativePath.matches(pattern.replace("*", ".*"));
  }

  private synchronized void processFile(Path filePath) {
    // Skip if already processed (synchronized for thread safety)
    if (processedFiles.contains(filePath)) {
      return;
    }

    // Skip if doesn't exist
    if (!Files.exists(filePath)) {
      getLogger().warn("File not found: {}", filePath);
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
      getLogger().debug("No minifier found for extension .{}: {}", extension, fileName);
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
        getLogger().info("Minified {}: {} → {} bytes ({}% reduction) in {} ms", fileName,
            originalSize, minifiedSize, String.format("%.1f", reductionPercent), duration);
      } else {
        getLogger().debug("No changes after minification: {}", fileName);
      }

      processedFiles.add(filePath);

    } catch (IOException e) {
      getLogger().warn("Error reading file {}: {}", filePath, e.getMessage());
    } catch (MinificationException e) {
      getLogger().warn("Error minifying file {}: {}", filePath, e.getMessage());
    } catch (Exception e) {
      getLogger().warn("Unexpected error processing {}: {}", filePath, e.getMessage());
    }
  }
}
