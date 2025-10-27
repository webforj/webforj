package com.webforj.minify.maven;

import com.google.gson.JsonSyntaxException;
import com.webforj.minify.common.AssetProcessor;
import com.webforj.minify.common.BuildLogger;
import com.webforj.minify.common.ResourceResolver;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
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
 * <p>Runs in the process-classes phase (after compilation, before WAR packaging).
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

  @Override
  public void execute() throws MojoExecutionException, MojoFailureException {
    if (skip) {
      getLog().info("Minification skipped (webforj.minify.skip=true)");
      return;
    }

    final long startTime = System.currentTimeMillis();
    getLog().info("Starting webforJ asset minification...");

    // Create logger adapter
    BuildLogger logger = new MavenBuildLogger(getLog());

    // Create processor
    AssetProcessor processor = new AssetProcessor(logger);

    // Load minifiers via SPI
    processor.getRegistry().loadMinifiers(getClass().getClassLoader());

    if (processor.getRegistry().getMinifierCount() == 0) {
      getLog().warn("No minifiers registered via SPI. Skipping minification.");
      getLog().warn("Ensure ph-css and/or closure-compiler are on the classpath.");
      return;
    }

    getLog().info("Discovered " + processor.getRegistry().getMinifierCount()
        + " minifier implementation(s) via SPI");

    // Process manifest file
    Path manifestPath = Paths.get(outputDirectory, "META-INF", "webforj-resources.json");
    if (Files.exists(manifestPath)) {
      getLog().info("Processing manifest: " + manifestPath);
      processManifest(processor, manifestPath);
    } else {
      getLog().debug("No manifest file found at " + manifestPath);
    }

    // Process additional configuration file
    Path configPath = Paths.get(project.getBasedir().getAbsolutePath(), "src", "main",
        RESOURCES_DIR, "META-INF", "webforj-minify.txt");
    if (Files.exists(configPath)) {
      getLog().info("Processing configuration file: " + configPath);
      // Use outputDirectory (target/classes) to process compiled resources, not source files
      Path resourcesRoot = Paths.get(outputDirectory);
      processor.processConfigFile(configPath, resourcesRoot);
    }

    long duration = System.currentTimeMillis() - startTime;
    getLog().info("Minification complete. Processed " + processor.getProcessedFileCount()
        + " file(s) in " + duration + " ms");
  }

  private void processManifest(AssetProcessor processor, Path manifestPath)
      throws MojoExecutionException {
    try {
      // Use outputDirectory (target/classes) to process compiled resources, not source files
      ResourceResolver resolver = new ResourceResolver(Paths.get(outputDirectory));
      processor.processManifest(manifestPath, resolver);
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
