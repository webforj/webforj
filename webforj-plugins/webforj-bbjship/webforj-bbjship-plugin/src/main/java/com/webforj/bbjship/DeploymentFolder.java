package com.webforj.bbjship;

import com.webforj.bbjship.DeploymentException.Reason;
import com.webforj.bbjship.foundation.DeploymentArchive;
import com.webforj.bbjship.foundation.FileTree;
import com.webforj.bbjship.foundation.PublishNamePolicy;
import java.io.IOException;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Stream;

/**
 * Reads and writes the deployment folders of BBjShip.
 *
 * <p>
 * Each application lives in {@code deployments/{name}/{id}}, where the id is a random UUID. The
 * file {@code deployments/{name}/current} names the id the registration uses. A development run
 * keeps the development properties of its archive, which name its class folders.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class DeploymentFolder {

  private static final String CURRENT = "current";
  private static final Logger LOGGER = System.getLogger(DeploymentFolder.class.getName());

  private DeploymentFolder() {}

  /**
   * Finds the deployment directory named by the {@code current} file of a publish name.
   *
   * @param root the deployment root path
   * @param name the publish name
   *
   * @return the current deployment directory, empty when the name is invalid, the file is missing
   *         or it names no deployment directory
   * @throws IOException if the {@code current} file cannot be read
   */
  public static Optional<Path> findCurrentDirectory(Path root, String name) throws IOException {
    if (!PublishNamePolicy.isValid(name)) {
      return Optional.empty();
    }

    Path current = root.resolve(name).resolve(CURRENT);
    if (!Files.isRegularFile(current, LinkOption.NOFOLLOW_LINKS)) {
      return Optional.empty();
    }

    String id = Files.readString(current).strip();
    if (!isCanonicalUuid(id)) {
      return Optional.empty();
    }

    Path directory = root.resolve(name).resolve(id);

    return Files.isDirectory(directory, LinkOption.NOFOLLOW_LINKS) ? Optional.of(directory)
        : Optional.empty();
  }

  /**
   * Reads the class folders of a development run from its deployment directory.
   *
   * @param directory the deployment directory
   *
   * @return the class folders in classpath order, empty when the directory holds no development run
   * @throws IOException if the development properties cannot be read
   */
  public static List<Path> readClassFolders(Path directory) throws IOException {
    return DeploymentArchive.findClassFolders(directory).orElse(List.of()).stream()
        .filter(folder -> !folder.isEmpty()).map(Path::of).toList();
  }

  /**
   * Reads the sources a deployment directory loads the application from.
   *
   * @param directory the deployment directory
   *
   * @return the class folders of a development run in classpath order, else the application jar,
   *         empty when the directory holds neither
   * @throws IOException if the development properties cannot be read
   */
  public static List<Path> readApplicationSources(Path directory) throws IOException {
    List<Path> classFolders = readClassFolders(directory);
    if (!classFolders.isEmpty()) {
      return classFolders;
    }

    Path jar = directory.resolve(DeploymentArchive.APPLICATION_ENTRY);

    return Files.isRegularFile(jar) ? List.of(jar) : List.of();
  }

  /**
   * Resolves the dependency jars of a deployment directory in classpath order.
   *
   * <p>
   * Only the jars directly inside the {@code dependencies} folder count, sorted by file name.
   * </p>
   *
   * @param directory the deployment directory
   *
   * @return the dependency jars
   * @throws DeploymentException if the dependencies folder is missing
   * @throws IOException if the {@code dependencies} folder cannot be read
   */
  public static List<Path> resolveDependencyJars(Path directory) throws IOException {
    Path dependencies = directory.resolve(DeploymentArchive.DEPENDENCIES_FOLDER);
    if (!Files.isDirectory(dependencies)) {
      throw new DeploymentException(Reason.INVALID_ARCHIVE,
          "BBj deployment dependencies folder is missing");
    }

    try (Stream<Path> files = Files.list(dependencies)) {
      return files.filter(Files::isRegularFile)
          .filter(path -> path.getFileName().toString().endsWith(".jar")).sorted().toList();
    }
  }

  /**
   * Formats the report of a folder that stayed because its files could not be removed.
   *
   * @param folder the folder that stayed
   * @param failure the removal failure
   *
   * @return the report
   */
  public static String formatKeptFolder(Path folder, IOException failure) {
    return "Not removed: " + folder + ". " + failure.getMessage();
  }

  static boolean isDeploymentId(Path directory) {
    return isCanonicalUuid(directory.getFileName().toString());
  }

  static void writeCurrent(Path directory) throws IOException {
    Path folder = directory.getParent();
    Path written = Files.createTempFile(folder, "." + CURRENT, ".tmp");
    try {
      Files.writeString(written, directory.getFileName().toString());
      Files.move(written, folder.resolve(CURRENT), StandardCopyOption.ATOMIC_MOVE,
          StandardCopyOption.REPLACE_EXISTING);
    } finally {
      Files.deleteIfExists(written);
    }
  }

  /**
   * Removes a deployment directory, the {@code current} file of its name and the folder of its name
   * once it is empty.
   *
   * @param directory the deployment directory
   *
   * @return the report naming the folder that stayed and why, empty when it is removed
   */
  static Optional<String> removeDeployment(Path directory) {
    Path folder = directory.getParent();
    try {
      FileTree.removeDirectory(directory);
      Files.deleteIfExists(folder.resolve(CURRENT));
      removeEmptyFolder(folder);
    } catch (IOException held) {
      // Windows refuses to delete a jar the BBjServices virtual machine has open, and BBjServices
      // keeps the jars of a removed classpath open until it restarts
      String report = formatKeptFolder(folder, held);
      LOGGER.log(Level.ERROR, report, held);
      return Optional.of(report);
    }

    return Optional.empty();
  }

  static void removeDeploymentDirectory(Path directory) throws IOException {
    FileTree.removeDirectory(directory);
    removeEmptyFolder(directory.getParent());
  }

  static void removeLeftoverDirectories(Path current) throws IOException {
    List<Path> leftovers;
    try (Stream<Path> entries = Files.list(current.getParent())) {
      leftovers = entries.filter(entry -> !entry.equals(current))
          .filter(entry -> Files.isDirectory(entry, LinkOption.NOFOLLOW_LINKS))
          .filter(DeploymentFolder::isDeploymentId).toList();
    }

    for (Path leftover : leftovers) {
      try {
        FileTree.removeDirectory(leftover);
      } catch (IOException failure) {
        LOGGER.log(Level.WARNING, "Cannot remove the previous BBj deployment folder " + leftover
            + ". The next deployment of this name removes it", failure);
      }
    }
  }

  static void removeEmptyFolder(Path folder) throws IOException {
    if (!Files.isDirectory(folder, LinkOption.NOFOLLOW_LINKS)) {
      return;
    }

    try (Stream<Path> children = Files.list(folder)) {
      if (children.findAny().isPresent()) {
        return;
      }
    }

    Files.delete(folder);
  }

  private static boolean isCanonicalUuid(String id) {
    try {
      return UUID.fromString(id).toString().equals(id);
    } catch (IllegalArgumentException invalid) {
      return false;
    }
  }
}
