package com.webforj.bbjship.foundation;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.attribute.PosixFilePermission;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.Comparator;
import java.util.Set;
import java.util.stream.Stream;

/**
 * Creates and removes file trees of BBjShip and its setup.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class FileTree {

  private static final String WORKSPACE_PREFIX = ".webforj-bbj-setup-";
  private static final String POSIX = "posix";
  private static final Set<PosixFilePermission> PRIVATE_FILE =
      PosixFilePermissions.fromString("rw-------");

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

  /**
   * Writes a file only its owner may read and write, replacing an existing file.
   *
   * @param file the file
   * @param content the text of the file
   *
   * @throws IOException if the file cannot be written
   */
  public static void writePrivateFile(Path file, String content) throws IOException {
    Files.writeString(file, content);
    if (file.getFileSystem().supportedFileAttributeViews().contains(POSIX)) {
      Files.setPosixFilePermissions(file, PRIVATE_FILE);
    }
  }

  static Path createPrivateDirectory(Path directory) throws IOException {
    if (Files.getFileStore(directory).supportsFileAttributeView(POSIX)) {
      return Files.createTempDirectory(directory, WORKSPACE_PREFIX,
          PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rwx------")));
    }

    return Files.createTempDirectory(directory, WORKSPACE_PREFIX);
  }

  static Path createPrivateFile(Path directory, String prefix, String suffix) throws IOException {
    if (Files.getFileStore(directory).supportsFileAttributeView(POSIX)) {
      return Files.createTempFile(directory, prefix, suffix,
          PosixFilePermissions.asFileAttribute(PRIVATE_FILE));
    }

    return Files.createTempFile(directory, prefix, suffix);
  }

  /**
   * Writes a copy of every folder and file of a tree into a folder, keeping the file attributes.
   *
   * @param source the root of the tree
   * @param destination the folder the copy is written to
   *
   * @throws IOException if a file cannot be read or written
   */
  static void writeTree(Path source, Path destination) throws IOException {
    try (Stream<Path> paths = Files.walk(source)) {
      for (Path file : paths.toList()) {
        Path relative = source.relativize(file);
        if (Files.isDirectory(file)) {
          Files.createDirectories(destination.resolve(relative));
        } else {
          Files.copy(file, destination.resolve(relative), StandardCopyOption.REPLACE_EXISTING,
              StandardCopyOption.COPY_ATTRIBUTES);
        }
      }
    }
  }
}
