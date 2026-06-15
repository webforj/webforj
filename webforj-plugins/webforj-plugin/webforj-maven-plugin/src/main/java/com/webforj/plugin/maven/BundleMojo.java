package com.webforj.plugin.maven;

import com.webforj.bundle.bun.BundlerExecution;
import java.nio.file.Path;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.ResolutionScope;

/**
 * Goal that bundles the project sources for a single build.
 *
 * <p>
 * Bound to {@code prepare-package} so the Bun output lands in {@code target/classes} before the
 * final JAR is produced. The bundler runs entirely on this plugin's classpath, so no bundler class
 * is ever placed on the application classpath or shipped with the application.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
@Mojo(name = "bundle", defaultPhase = LifecyclePhase.PREPARE_PACKAGE,
    requiresDependencyResolution = ResolutionScope.COMPILE_PLUS_RUNTIME, threadSafe = true)
public class BundleMojo extends AbstractBundlerMojo {

  @Override
  public void execute() throws MojoExecutionException {
    BundlerExecution execution = createExecution();
    try {
      Path outputDir = execution.run(createRequest(), new MavenBundleLogger(getLog()));
      if (outputDir != null) {
        getLog().info("bundle output written to " + outputDir);
      }

    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();
      throw new MojoExecutionException("bundle was interrupted", e);
    } catch (Exception e) {
      throw new MojoExecutionException("bundle failed: " + e.getMessage(), e);
    }
  }
}
