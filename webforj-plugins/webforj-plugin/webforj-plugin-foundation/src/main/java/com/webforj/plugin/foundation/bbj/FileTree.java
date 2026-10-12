package com.webforj.plugin.foundation.bbj;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.Comparator;

/**
 * Creates and removes file trees of BBjShip and its setup.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class FileTree {

  private static final String WORKSPACE_PREFIX = ".webforj-bbj-setup-";

  private FileTree() {}

  /**
   * Removes a directory with every file inside it, without following symbolic links.
   *
   * @param directory the directory to remove, ignored when it does not exist
   *
   * @throws IOException if a file cannot be removed
   */
  public static void removeDirectory(Path directory) throws IOException {
    if (!Files.exists(directory, LinkOption.NOFOLLOW_LINKS)) {
      return;
    }

    try (var paths = Files.walk(directory)) {
      for (Path path : paths.sorted(Comparator.reverseOrder()).toList()) {
        Files.delete(path);
      }
    }
  }

  static Path createPrivateDirectory(Path directory) throws IOException {
    if (Files.getFileStore(directory).supportsFileAttributeView("posix")) {
      return Files.createTempDirectory(directory, WORKSPACE_PREFIX,
          PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rwx------")));
    }

    return Files.createTempDirectory(directory, WORKSPACE_PREFIX);
  }

  static Path createPrivateFile(Path directory, String prefix, String suffix) throws IOException {
    if (Files.getFileStore(directory).supportsFileAttributeView("posix")) {
      return Files.createTempFile(directory, prefix, suffix,
          PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rw-------")));
    }

    return Files.createTempFile(directory, prefix, suffix);
  }
}
