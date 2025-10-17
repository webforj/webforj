package com.webforj.minify.maven;

import com.google.gson.Gson;
import com.google.gson.JsonObject;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.webforj.minify.common.AssetMinifier;
import com.webforj.minify.common.MinifierRegistry;
import com.webforj.minify.common.MinificationException;
import com.webforj.minify.common.ResourceResolver;

import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashSet;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Stream;

/**
 * Maven Mojo that minifies webforJ assets during the build process.
 *
 * Runs in the process-classes phase (after compilation, before WAR packaging).
 */
@Mojo(name = "minify", defaultPhase = LifecyclePhase.PROCESS_CLASSES, threadSafe = true)
public class MinifyMojo extends AbstractMojo {

  private static final Pattern MIN_FILE_PATTERN = Pattern.compile(".*\\.min\\.(css|js)$");

  @Parameter(defaultValue = "${project}", readonly = true, required = true)
  private MavenProject project;

  @Parameter(defaultValue = "${project.build.outputDirectory}", readonly = true)
  private String outputDirectory;

  @Parameter(property = "webforj.minify.skip", defaultValue = "false")
  private boolean skip;

  private final Gson gson = new Gson();
  private final MinifierRegistry registry = new MinifierRegistry();
  private final Set<Path> processedFiles = new HashSet<>();

  @Override
  public void execute() throws MojoExecutionException, MojoFailureException {
    if (skip) {
      getLog().info("Minification skipped");
      return;
    }

    getLog().info("Starting webforJ asset minification...");

    // Load minifiers via SPI
    registry.loadMinifiers(getClass().getClassLoader());

    if (registry.getMinifierCount() == 0) {
      getLog().warn("No minifiers registered. Skipping minification.");
      return;
    }

    // Process manifest file
    Path manifestPath = Paths.get(outputDirectory, "META-INF", "webforj-resources.json");
    if (Files.exists(manifestPath)) {
      processManifest(manifestPath);
    } else {
      getLog().debug("No manifest file found at " + manifestPath);
    }

    // Process additional configuration file
    Path configPath = Paths.get(project.getBasedir().getAbsolutePath(), "src", "main", "resources",
                                 "META-INF", "webforj-minify.txt");
    if (Files.exists(configPath)) {
      processConfigFile(configPath);
    }

    getLog().info("Minification complete. Processed " + processedFiles.size() + " file(s)");
  }

  private void processManifest(Path manifestPath) {
    try {
      String content = Files.readString(manifestPath, StandardCharsets.UTF_8);
      JsonObject manifest = gson.fromJson(content, JsonObject.class);

      if (manifest.has("resources")) {
        JsonArray resources = manifest.getAsJsonArray("resources");
        ResourceResolver resolver = new ResourceResolver(
          Paths.get(project.getBasedir().getAbsolutePath(), "src", "main", "resources")
        );

        for (JsonElement element : resources) {
          JsonObject resource = element.getAsJsonObject();
          String url = resource.get("url").getAsString();

          Path filePath = resolver.resolve(url);
          processFile(filePath);
        }
      }
    } catch (IOException e) {
      getLog().error("Failed to read manifest file: " + e.getMessage(), e);
    } catch (Exception e) {
      getLog().error("Malformed manifest file: " + e.getMessage(), e);
      throw new RuntimeException("Malformed manifest file", e);
    }
  }

  private void processConfigFile(Path configPath) {
    try {
      Path resourcesRoot = Paths.get(project.getBasedir().getAbsolutePath(), "src", "main", "resources");

      Files.lines(configPath, StandardCharsets.UTF_8)
        .map(String::trim)
        .filter(line -> !line.isEmpty() && !line.startsWith("#"))
        .forEach(pattern -> {
          if (pattern.startsWith("!")) {
            // Exclusion pattern - not yet implemented in this basic version
            getLog().debug("Exclusion pattern: " + pattern);
          } else {
            // Inclusion pattern - find matching files
            processGlobPattern(resourcesRoot, pattern);
          }
        });
    } catch (IOException e) {
      getLog().warn("Failed to read config file: " + e.getMessage());
    }
  }

  private void processGlobPattern(Path root, String pattern) {
    try (Stream<Path> paths = Files.walk(root)) {
      paths.filter(Files::isRegularFile)
        .filter(p -> matchesGlob(root, p, pattern))
        .forEach(this::processFile);
    } catch (IOException e) {
      getLog().warn("Error processing glob pattern " + pattern + ": " + e.getMessage());
    }
  }

  private boolean matchesGlob(Path root, Path file, String pattern) {
    String relativePath = root.relativize(file).toString().replace('\\', '/');
    return relativePath.matches(pattern.replace("*", ".*"));
  }

  private void processFile(Path filePath) {
    // Skip if already processed
    if (processedFiles.contains(filePath)) {
      return;
    }

    // Skip if doesn't exist
    if (!Files.exists(filePath)) {
      getLog().warn("File not found: " + filePath);
      return;
    }

    // Skip if already minified
    if (MIN_FILE_PATTERN.matcher(filePath.toString()).matches()) {
      getLog().debug("Skipping already minified file: " + filePath);
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
      getLog().debug("No minifier found for extension: " + extension);
      return;
    }

    // Minify the file
    try {
      String content = Files.readString(filePath, StandardCharsets.UTF_8);
      String minified = minifier.minify(content, filePath);

      // Write minified content back
      Files.writeString(filePath, minified, StandardCharsets.UTF_8);

      processedFiles.add(filePath);
      getLog().info("Minified: " + filePath);

    } catch (IOException e) {
      getLog().warn("Error reading file " + filePath + ": " + e.getMessage());
    } catch (MinificationException e) {
      getLog().warn("Error minifying file " + filePath + ": " + e.getMessage());
    }
  }
}
