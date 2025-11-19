package com.webforj.minify.maven;

import com.google.gson.JsonSyntaxException;
import com.webforj.minify.common.AssetProcessor;
import com.webforj.minify.common.BuildLogger;
import com.webforj.minify.common.ResourceResolver;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;

/**
 * Maven Mojo that minifies webforJ assets during the build process.
 *
 * <p>
 * Runs in the process-classes phase (after compilation, before WAR packaging).
 * </p>
 *
 * @author Kevin Hagel
 */
@Mojo(name = "minify", defaultPhase = LifecyclePhase.PROCESS_CLASSES, threadSafe = true)
public class MinifyMojo extends AbstractMojo {

  private static final String RESOURCES_DIR = "resources";

  @Parameter(defaultValue = "${project}", readonly = true, required = true)
  private MavenProject project;

  @Parameter(defaultValue = "${project.build.outputDirectory}", readonly = true)
  private String outputDirectory;

  @Parameter(property = "webforj.minify.skip", defaultValue = "false")
  private boolean skip;

  @Parameter
  private java.util.Map<String, java.util.Map<String, String>> minifierConfigurations;

  @Override
  public void execute() throws MojoExecutionException, MojoFailureException {
    if (skip) {
      getLog().info("Minification skipped (webforj.minify.skip=true)");
      return;
    }

    final long startTime = System.currentTimeMillis();
    getLog().info("Starting webforJ asset minification...");

    // Log configuration if present
    if (minifierConfigurations != null && !minifierConfigurations.isEmpty()) {
      getLog().debug("Minifier configurations provided: " + minifierConfigurations.keySet());
      for (Map.Entry<String, java.util.Map<String, String>> entry : minifierConfigurations
          .entrySet()) {
        getLog().debug("Configuration [" + entry.getKey() + "]: " + entry.getValue());
      }
    } else {
      getLog().debug("No minifier configurations provided");
    }

    // Create logger adapter
    BuildLogger logger = new MavenBuildLogger(getLog());

    // Create processor
    AssetProcessor processor = new AssetProcessor(logger);

    // Load minifiers via SPI
    processor.getRegistry().loadMinifiers(getClass().getClassLoader());

    // Set build logger on registry for configuration logging
    processor.getRegistry().setBuildLogger(logger);

    if (processor.getRegistry().getMinifierCount() == 0) {
      getLog().warn("No minifiers registered via SPI. Skipping minification.");
      getLog().warn("Ensure ph-css and/or closure-compiler are on the classpath.");
      return;
    }

    getLog().info("Discovered " + processor.getRegistry().getMinifierCount()
        + " minifier implementation(s) via SPI");

    // Configure minifiers with provided configuration
    if (minifierConfigurations != null && !minifierConfigurations.isEmpty()) {
      // Convert Map<String, Map<String, String>> to Map<String, Object>
      Map<String, Object> config = new java.util.HashMap<>();
      for (Map.Entry<String, java.util.Map<String, String>> entry : minifierConfigurations
          .entrySet()) {
        config.put(entry.getKey(), entry.getValue());
      }
      getLog().debug("Applying configuration to minifiers: " + config.keySet());
      processor.getRegistry().configureMinifiers(config);
      getLog().info("Applied configuration to minifiers");
    }

    // Collect files from both manifest and config file
    // Config file rules ALWAYS take priority
    Path resourcesRoot = Paths.get(outputDirectory);
    Path configPath = Paths.get(project.getBasedir().getAbsolutePath(), "src", "main",
        RESOURCES_DIR, "META-INF", "webforj-minify.txt");
    Path manifestPath = Paths.get(outputDirectory, "META-INF", "webforj-resources.json");

    java.util.Set<java.nio.file.Path> allFiles = new java.util.HashSet<>();

    // 1. Collect files from manifest
    if (Files.exists(manifestPath)) {
      getLog().info("Processing manifest: " + manifestPath);
      allFiles.addAll(collectManifestFiles(processor, manifestPath));
    } else {
      getLog().debug("No manifest file found at " + manifestPath);
    }

    // 2. Collect files from config file and apply its exclusions to ALL files
    if (Files.exists(configPath)) {
      getLog().info("Processing configuration file: " + configPath);
      allFiles.addAll(processor.collectConfigFiles(configPath, resourcesRoot));
      // Apply config file exclusions to manifest files too (config takes priority)
      processor.applyConfigExclusions(configPath, resourcesRoot, allFiles);
    }

    // 3. Process all collected files
    processor.processFiles(allFiles);

    long duration = System.currentTimeMillis() - startTime;
    getLog().info("Minification complete. Processed " + processor.getProcessedFileCount()
        + " file(s) in " + duration + " ms");
  }

  private java.util.Set<java.nio.file.Path> collectManifestFiles(AssetProcessor processor,
      Path manifestPath) throws MojoExecutionException {
    try {
      // Use outputDirectory (target/classes) to process compiled resources, not source files
      ResourceResolver resolver = new ResourceResolver(Paths.get(outputDirectory));
      return processor.collectManifestFiles(manifestPath, resolver);
    } catch (JsonSyntaxException e) {
      throw new MojoExecutionException(
          "Malformed manifest file - check META-INF/webforj-resources.json", e);
    } catch (RuntimeException e) {
      throw new MojoExecutionException("Asset minification failed: " + e.getMessage(), e);
    }
  }

  /**
   * Adapter to bridge Maven's Log to BuildLogger interface.
   */
  private static class MavenBuildLogger implements BuildLogger {
    private final org.apache.maven.plugin.logging.Log log;

    MavenBuildLogger(org.apache.maven.plugin.logging.Log log) {
      this.log = log;
    }

    @Override
    public void info(String message) {
      log.info(message);
    }

    @Override
    public void warn(String message) {
      log.warn(message);
    }

    @Override
    public void debug(String message) {
      log.debug(message);
    }

    @Override
    public void error(String message, Throwable throwable) {
      log.error(message, throwable);
    }
  }
}
