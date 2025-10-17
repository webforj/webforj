package com.webforj.minify.gradle;

import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.webforj.minify.common.AssetMinifier;
import com.webforj.minify.common.MinificationException;
import com.webforj.minify.common.MinifierRegistry;
import com.webforj.minify.common.ResourceResolver;

import org.gradle.api.DefaultTask;
import org.gradle.api.provider.Property;
import org.gradle.api.tasks.Input;
import org.gradle.api.tasks.TaskAction;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashSet;
import java.util.Set;
import java.util.regex.Pattern;

/**
 * Gradle task that minifies webforJ assets.
 */
public abstract class MinifyTask extends DefaultTask {

  private static final Pattern MIN_FILE_PATTERN = Pattern.compile(".*\\.min\\.(css|js)$");

  private final Gson gson = new Gson();
  private final MinifierRegistry registry = new MinifierRegistry();
  private final Set<Path> processedFiles = new HashSet<>();

  @Input
  public abstract Property<Boolean> getEnabled();

  public MinifyTask() {
    getEnabled().convention(true);
  }

  @TaskAction
  public void minify() {
    if (!getEnabled().get()) {
      getLogger().info("Minification skipped");
      return;
    }

    getLogger().info("Starting webforJ asset minification...");

    // Load minifiers via SPI
    registry.loadMinifiers(getClass().getClassLoader());

    if (registry.getMinifierCount() == 0) {
      getLogger().warn("No minifiers registered. Skipping minification.");
      return;
    }

    // Get output directory
    File outputDir = new File(getProject().getBuildDir(), "resources/main");
    if (!outputDir.exists()) {
      getLogger().warn("Output directory does not exist: " + outputDir);
      return;
    }

    // Process manifest file
    Path manifestPath = Paths.get(outputDir.getAbsolutePath(), "META-INF", "webforj-resources.json");
    if (Files.exists(manifestPath)) {
      processManifest(manifestPath);
    } else {
      getLogger().debug("No manifest file found at " + manifestPath);
    }

    // Process additional configuration file
    Path configPath = Paths.get(getProject().getProjectDir().getAbsolutePath(), "src", "main",
                                 "resources", "META-INF", "webforj-minify.txt");
    if (Files.exists(configPath)) {
      processConfigFile(configPath);
    }

    getLogger().info("Minification complete. Processed " + processedFiles.size() + " file(s)");
  }

  private void processManifest(Path manifestPath) {
    try {
      String content = Files.readString(manifestPath, StandardCharsets.UTF_8);
      JsonObject manifest = gson.fromJson(content, JsonObject.class);

      if (manifest.has("resources")) {
        JsonArray resources = manifest.getAsJsonArray("resources");
        ResourceResolver resolver = new ResourceResolver(
          Paths.get(getProject().getProjectDir().getAbsolutePath(), "src", "main", "resources")
        );

        for (JsonElement element : resources) {
          JsonObject resource = element.getAsJsonObject();
          String url = resource.get("url").getAsString();

          Path filePath = resolver.resolve(url);
          processFile(filePath);
        }
      }
    } catch (IOException e) {
      getLogger().error("Failed to read manifest file: " + e.getMessage(), e);
    } catch (Exception e) {
      getLogger().error("Malformed manifest file: " + e.getMessage(), e);
      throw new RuntimeException("Malformed manifest file", e);
    }
  }

  private void processConfigFile(Path configPath) {
    try {
      Path resourcesRoot = Paths.get(getProject().getProjectDir().getAbsolutePath(), "src", "main", "resources");

      Files.lines(configPath, StandardCharsets.UTF_8)
        .map(String::trim)
        .filter(line -> !line.isEmpty() && !line.startsWith("#"))
        .forEach(pattern -> {
          if (pattern.startsWith("!")) {
            // Exclusion pattern
            getLogger().debug("Exclusion pattern: " + pattern);
          } else {
            // Inclusion pattern
            getLogger().debug("Inclusion pattern: " + pattern);
          }
        });
    } catch (IOException e) {
      getLogger().warn("Failed to read config file: " + e.getMessage());
    }
  }

  private void processFile(Path filePath) {
    // Skip if already processed
    if (processedFiles.contains(filePath)) {
      return;
    }

    // Skip if doesn't exist
    if (!Files.exists(filePath)) {
      getLogger().warn("File not found: " + filePath);
      return;
    }

    // Skip if already minified
    if (MIN_FILE_PATTERN.matcher(filePath.toString()).matches()) {
      getLogger().debug("Skipping already minified file: " + filePath);
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
      getLogger().debug("No minifier found for extension: " + extension);
      return;
    }

    // Minify the file
    try {
      String content = Files.readString(filePath, StandardCharsets.UTF_8);
      String minified = minifier.minify(content, filePath);

      // Write minified content back
      Files.writeString(filePath, minified, StandardCharsets.UTF_8);

      processedFiles.add(filePath);
      getLogger().info("Minified: " + filePath);

    } catch (IOException e) {
      getLogger().warn("Error reading file " + filePath + ": " + e.getMessage());
    } catch (MinificationException e) {
      getLogger().warn("Error minifying file " + filePath + ": " + e.getMessage());
    }
  }
}
