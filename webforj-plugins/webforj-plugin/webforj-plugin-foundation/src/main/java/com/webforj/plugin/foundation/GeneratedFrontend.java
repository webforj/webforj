package com.webforj.plugin.foundation;

import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Optional;

/**
 * The generated frontend the bundler writes under the source root, shared by the clean step of
 * every build tool so a clean leaves none behind.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public final class GeneratedFrontend {

  private static final String GENERATED_DIR = "generated";

  private GeneratedFrontend() {}

  /**
   * Resolves the generated directory under the given source root.
   *
   * @param sourceRoot the bundle source root
   * @return the generated directory
   */
  public static Path resolveDirectory(Path sourceRoot) {
    return sourceRoot.resolve(GENERATED_DIR);
  }

  /**
   * Removes the generated directory under the given source root.
   *
   * @param sourceRoot the bundle source root
   * @return the removed directory, or empty when there was nothing to remove
   * @throws IOException if the directory cannot be removed
   */
  public static Optional<Path> remove(Path sourceRoot) throws IOException {
    Path generated = resolveDirectory(sourceRoot);
    if (!Files.isDirectory(generated)) {
      return Optional.empty();
    }

    deleteRecursively(generated);

    return Optional.of(generated);
  }

  private static void deleteRecursively(Path directory) throws IOException {
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
