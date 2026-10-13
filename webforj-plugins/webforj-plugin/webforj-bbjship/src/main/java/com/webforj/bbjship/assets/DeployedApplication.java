package com.webforj.bbjship.assets;

import com.webforj.bbjship.ApplicationConfiguration;
import com.webforj.bbjship.DeploymentFolder;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.jar.JarFile;

/**
 * Reads resources and the asset configuration of one published application.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
final class DeployedApplication {

  private final List<Path> folders;
  private final Map<String, Path> jarIndex;
  private final ResourceIndexes indexes;
  private final ApplicationConfiguration configuration;

  private DeployedApplication(List<Path> folders, Map<String, Path> jarIndex,
      ResourceIndexes indexes, ApplicationConfiguration configuration) {
    this.folders = List.copyOf(folders);
    this.jarIndex = jarIndex;
    this.indexes = indexes;
    this.configuration = configuration;
  }

  /**
   * Loads the application of a deployment directory.
   *
   * <p>
   * The application is read from the class folders of a development run in classpath order, else
   * from the application jar. The jars are scanned once per deployment directory, by the indexes.
   * </p>
   *
   * @param directory the deployment directory
   * @param indexes the indexes that hold the jar entries of the deployments
   *
   * @return the application
   * @throws IOException if the directory holds neither a development run nor the application jar,
   *         the dependency jars cannot be listed or scanned or the configuration cannot be read
   */
  static DeployedApplication load(Path directory, ResourceIndexes indexes) throws IOException {
    List<Path> application = DeploymentFolder.readApplicationSources(directory);
    if (application.isEmpty()) {
      throw new IOException("BBj deployment application JAR is missing");
    }

    List<Path> sources = new ArrayList<>(application);
    sources.addAll(DeploymentFolder.resolveDependencyJars(directory));
    ApplicationConfiguration configuration =
        ApplicationConfiguration.load(application.toArray(Path[]::new));
    List<Path> folders = sources.stream().filter(Files::isDirectory).toList();
    List<Path> jars = sources.stream().filter(Files::isRegularFile).toList();

    return new DeployedApplication(folders, indexes.get(directory, jars), indexes, configuration);
  }

  String getAssetsPrefix() {
    return configuration.getAssetsPrefix();
  }

  String getIconsPrefix() {
    return configuration.getIconsPrefix();
  }

  List<String> getIndexes() {
    return configuration.getIndexes();
  }

  List<String> getExtensions() {
    return configuration.getExtensions();
  }

  String getCacheControl() {
    return configuration.getCacheControl();
  }

  Map<String, String> getMimeExtensions() {
    return configuration.getMimeExtensions();
  }

  /**
   * Reads a resource from the class folders, then from the application jar and the dependencies in
   * classpath order.
   *
   * <p>
   * Only the jar that holds the resource is opened, and no jar stays open after the read.
   * </p>
   *
   * @param name the resource name inside the application and its dependencies
   *
   * @return the resource bytes, an empty array for an empty file, or an empty optional when no
   *         source contains the resource or the name is a folder
   * @throws IOException if a jar or a file of a class folder cannot be read
   */
  Optional<byte[]> readResource(String name) throws IOException {
    if (name.endsWith("/")) {
      return Optional.empty();
    }

    for (Path folder : folders) {
      Optional<Path> file = findFolderFile(folder, name);
      if (file.isPresent()) {
        return Files.isDirectory(file.get()) ? Optional.empty()
            : Optional.of(Files.readAllBytes(file.get()));
      }
    }

    Path jar = jarIndex.get(name);
    if (jar != null) {
      return Optional.of(readJarEntry(jar, name));
    }

    return Optional.empty();
  }

  private byte[] readJarEntry(Path path, String name) throws IOException {
    try (JarFile jar = indexes.open(path);
        InputStream input = jar.getInputStream(jar.getJarEntry(name))) {
      return input.readAllBytes();
    }
  }

  private static Optional<Path> findFolderFile(Path folder, String name) {
    Path base = folder.toAbsolutePath().normalize();
    Path file = base.resolve(name).normalize();
    if (!file.startsWith(base) || !Files.exists(file)) {
      return Optional.empty();
    }

    return Optional.of(file);
  }
}
