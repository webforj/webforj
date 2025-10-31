package com.webforj.minify.gradle;

import com.google.gson.JsonSyntaxException;
import com.webforj.minify.common.AssetProcessor;
import com.webforj.minify.common.BuildLogger;
import com.webforj.minify.common.ResourceResolver;
import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashSet;
import java.util.Set;
import javax.inject.Inject;
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
 *
 * @author Kevin Hagel
 */
public abstract class MinifyTask extends DefaultTask {

  /**
   * Creates a new MinifyTask instance.
   */
  @Inject
  protected MinifyTask() {
    setGroup("webforJ");
    setDescription("Minifies webforJ assets");
  }

  /**
   * Gets the output directory where compiled classes are located.
   *
   * @return the output directory property
   */
  @InputDirectory
  public abstract DirectoryProperty getOutputDirectory();

  /**
   * Gets the resources directory containing source resources.
   *
   * @return the resources directory property
   */
  @InputDirectory
  public abstract DirectoryProperty getResourcesDirectory();

  /**
   * Gets whether minification should be skipped.
   *
   * @return the skip property
   */
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

    // Create logger adapter
    BuildLogger logger = new GradleBuildLogger(getLogger());

    // Create processor
    AssetProcessor processor = new AssetProcessor(logger);

    // Load minifiers via SPI
    processor.getRegistry().loadMinifiers(getClass().getClassLoader());

    if (processor.getRegistry().getMinifierCount() == 0) {
      getLogger().warn("No minifiers registered via SPI. Skipping minification.");
      getLogger().warn("Ensure ph-css and/or closure-compiler are on the classpath.");
      return;
    }

    getLogger().info("Discovered {} minifier implementation(s) via SPI",
        processor.getRegistry().getMinifierCount());

    // Collect files to process
    Set<Path> filesToProcess = new HashSet<>();

    // Collect files from manifest
    File outputDir = getOutputDirectory().get().getAsFile();
    Path manifestPath =
        Paths.get(outputDir.getAbsolutePath(), "META-INF", "webforj-resources.json");

    if (Files.exists(manifestPath)) {
      getLogger().info("Processing manifest: {}", manifestPath);
      collectFromManifest(processor, manifestPath, filesToProcess);
    } else {
      getLogger().debug("No manifest file found at {}", manifestPath);
    }

    // Collect files from configuration file
    File resourcesDir = getResourcesDirectory().get().getAsFile();
    Path configPath = Paths.get(resourcesDir.getAbsolutePath(), "META-INF", "webforj-minify.txt");

    if (Files.exists(configPath)) {
      getLogger().info("Processing configuration file: {}", configPath);
      Set<Path> configFiles = processor.collectConfigFiles(configPath, resourcesDir.toPath());
      filesToProcess.addAll(configFiles);
    }

    // Process all collected files
    processor.processFiles(filesToProcess);

    long duration = System.currentTimeMillis() - startTime;
    getLogger().info("Minification complete. Processed {} file(s) in {} ms",
        processor.getProcessedFileCount(), duration);
  }

  private void collectFromManifest(AssetProcessor processor, Path manifestPath,
      Set<Path> filesToProcess) {
    try {
      File resourcesDir = getResourcesDirectory().get().getAsFile();
      ResourceResolver resolver = new ResourceResolver(resourcesDir.toPath());
      Set<Path> manifestFiles = processor.collectManifestFiles(manifestPath, resolver);
      filesToProcess.addAll(manifestFiles);
    } catch (JsonSyntaxException e) {
      throw new GradleException(
          "Malformed manifest file - check META-INF/webforj-resources.json", e);
    }
  }

  /**
   * Adapter to bridge Gradle's Logger to BuildLogger interface.
   */
  private static class GradleBuildLogger implements BuildLogger {
    private final org.gradle.api.logging.Logger logger;

    GradleBuildLogger(org.gradle.api.logging.Logger logger) {
      this.logger = logger;
    }

    @Override
    public void info(String message) {
      logger.info(message);
    }

    @Override
    public void warn(String message) {
      logger.warn(message);
    }

    @Override
    public void debug(String message) {
      logger.debug(message);
    }

    @Override
    public void error(String message, Throwable throwable) {
      logger.error(message, throwable);
    }
  }
}
