package com.webforj.maven;

import java.io.File;
import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
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

  private static final String GENERATED_DIR = "generated";

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
    Path generated = sourceRoot.toPath().resolve(GENERATED_DIR);
    if (!Files.isDirectory(generated)) {
      return;
    }

    try {
      deleteDirectory(generated);
      getLog().info("removed " + generated);
    } catch (IOException e) {
      throw new MojoExecutionException("failed to clean " + generated, e);
    }
  }

  private void deleteDirectory(Path directory) throws IOException {
    Files.walkFileTree(directory, new SimpleFileVisitor<Path>() {
      @Override
      public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
        Files.delete(file);

        return FileVisitResult.CONTINUE;
      }

      @Override
      public FileVisitResult postVisitDirectory(Path dir, IOException failure) throws IOException {
        if (failure != null) {
          throw failure;
        }
        Files.delete(dir);

        return FileVisitResult.CONTINUE;
      }
    });
  }
}
