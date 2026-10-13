package com.webforj.bbjship.foundation;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.util.Set;
import java.util.stream.Stream;

/**
 * Locates the files of the BBjShip plugin and the files BBjShip writes at runtime inside a BBj
 * installation.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class BBjShipPlugin {

  static final String LIBRARY = "lib/webforj-bbjship.jar";
  private static final String HOME_PROPERTY = "basis.BBjHome";
  private static final String PLUGINS = "plugins";
  private static final String PLUGIN_FOLDER = "webforj-bbjship";
  private static final String RUNTIME_FOLDER = ".webforj-bbjship-deployments";
  private static final String DEPLOYMENTS = "deployments";
  private static final String STATIC_FILES = "htdocs";
  private static final String ASSETS = "assets";
  private static final String DEVELOPMENT_SWITCH = "development.enabled";
  static final Set<String> RUNTIME_NAMES =
      Set.of(DEPLOYMENTS, STATIC_FILES, ASSETS, DEVELOPMENT_SWITCH, RUNTIME_FOLDER);

  private final Path home;
  private final Path directory;
  private final Path runtime;

  /**
   * Creates the plugin locator.
   *
   * @param home the BBj installation folder
   */
  public BBjShipPlugin(Path home) {
    this.home = home.toAbsolutePath().normalize();
    this.directory = this.home.resolve(PLUGINS).resolve(PLUGIN_FOLDER);
    this.runtime = this.home.resolve(PLUGINS).resolve(RUNTIME_FOLDER);
  }

  /**
   * Creates the plugin locator of the BBj installation named by the {@code basis.BBjHome} system
   * property, which BBjServices and the BBjShip launchers set.
   *
   * @return the plugin locator
   * @throws NullPointerException if the system property is not set
   */
  public static BBjShipPlugin createFromBBjHome() {
    return new BBjShipPlugin(Path.of(System.getProperty(HOME_PROPERTY)));
  }

  /**
   * Resolves the file whose presence enables development runs, beside a deployments folder.
   *
   * @param deployments the deployments folder of BBjShip
   *
   * @return the development switch file
   */
  public static Path resolveDevelopmentSwitch(Path deployments) {
    return deployments.resolveSibling(DEVELOPMENT_SWITCH);
  }

  /**
   * Returns the BBj installation folder.
   *
   * @return the BBj installation folder
   */
  public Path getHome() {
    return home;
  }

  /**
   * Returns the folder of the BBjShip plugin.
   *
   * @return the plugin folder
   */
  public Path getDirectory() {
    return directory;
  }

  /**
   * Returns the folder that holds every file BBjShip writes at runtime.
   *
   * @return the runtime folder
   */
  public Path getRuntime() {
    return runtime;
  }

  /**
   * Returns the BBjShip library of the plugin.
   *
   * @return the BBjShip jar
   */
  public Path getLibrary() {
    return directory.resolve(LIBRARY);
  }

  /**
   * Returns the folder of the libraries BBj ships.
   *
   * @return the BBj library folder
   */
  public Path getBBjLibraries() {
    return home.resolve(".lib");
  }

  /**
   * Returns the configuration file of the BBj installation.
   *
   * @return the BBj configuration file
   */
  public Path getConfigFile() {
    return home.resolve("cfg").resolve("config.bbx");
  }

  /**
   * Returns the folder of the deployments of BBjShip.
   *
   * @return the deployments folder
   */
  public Path getDeployments() {
    return runtime.resolve(DEPLOYMENTS);
  }

  /**
   * Returns the document folder of the BBjShip web context.
   *
   * @return the static files folder
   */
  public Path getStaticFiles() {
    return runtime.resolve(STATIC_FILES);
  }

  /**
   * Returns the document folder of the assets web context.
   *
   * @return the assets folder
   */
  public Path getAssets() {
    return runtime.resolve(ASSETS);
  }

  /**
   * Returns the file whose presence enables development runs.
   *
   * @return the development switch file
   */
  public Path getDevelopmentSwitch() {
    return resolveDevelopmentSwitch(getDeployments());
  }

  /**
   * Reads the BBjShip version from the version file of the plugin.
   *
   * @return the BBjShip version
   * @throws IOException if the version file cannot be read or names no version
   */
  public String readVersion() throws IOException {
    return BBjShipVersionFile.readVersion(directory);
  }

  /**
   * Refuses a plugins folder, plugin folder or runtime folder that is a symbolic link or a file,
   * and a plugin folder that holds a symbolic link.
   *
   * @throws IOException if a folder is a symbolic link or a file, or the plugin folder holds a
   *         symbolic link
   */
  public void applyOwnershipValidation() throws IOException {
    for (Path folder : new Path[] {directory.getParent(), directory, runtime}) {
      if (Files.isSymbolicLink(folder)) {
        throw new IOException(
            "The plugin directory is a symbolic link. Install the plugin into a regular directory");
      }

      if (Files.exists(folder, LinkOption.NOFOLLOW_LINKS) && !Files.isDirectory(folder)) {
        throw new IOException("BBj plugin destination must be a directory without symbolic links");
      }
    }

    if (!Files.isDirectory(directory)) {
      return;
    }

    try (Stream<Path> paths = Files.walk(directory)) {
      if (paths.anyMatch(Files::isSymbolicLink)) {
        throw new IOException("BBj plugin destination must not contain symbolic links");
      }
    }
  }
}
