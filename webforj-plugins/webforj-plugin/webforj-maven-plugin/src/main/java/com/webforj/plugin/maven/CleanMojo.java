package com.webforj.plugin.maven;

import com.webforj.plugin.foundation.GeneratedFrontend;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;

/**
 * Removes the generated frontend the bundler writes outside the build directory, so {@code mvn
 * clean} leaves none behind.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
@Mojo(name = "clean", defaultPhase = LifecyclePhase.CLEAN, threadSafe = true)
public class CleanMojo extends AbstractMojo {

  /**
   * Source root for bundle entry files.
   */
  @Parameter(property = "webforj.bundler.sourceRoot",
      defaultValue = "${project.basedir}/src/main/frontend")
  protected File sourceRoot;

  /**
   * {@inheritDoc}
   */
  @Override
  public void execute() throws MojoExecutionException {
    Path source = sourceRoot.toPath();
    try {
      GeneratedFrontend.remove(source).ifPresent(removed -> getLog().info("removed " + removed));
    } catch (IOException e) {
      throw new MojoExecutionException(
          "failed to clean " + GeneratedFrontend.resolveDirectory(source), e);
    }
  }
}
