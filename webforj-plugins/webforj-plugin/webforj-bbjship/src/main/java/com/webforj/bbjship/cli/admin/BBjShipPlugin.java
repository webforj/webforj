package com.webforj.bbjship.cli.admin;

import com.webforj.plugin.foundation.bbj.BBjShipVersionFile;
import com.webforj.plugin.foundation.bbj.DeploymentArchive;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * Locates the files of the BBjShip plugin inside a BBj installation.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class BBjShipPlugin {

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
    this.directory = this.home.resolve("plugins").resolve("webforj-bbjship");
    this.runtime = this.home.resolve("plugins").resolve(DeploymentArchive.RUNTIME_DIRECTORY);
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

  Path getLibrary() {
    return directory.resolve("lib").resolve("webforj-bbjship.jar");
  }

  Path getBBjLibraries() {
    return home.resolve(".lib");
  }

  Path getConfigFile() {
    return home.resolve("cfg").resolve("config.bbx");
  }

  /**
   * Returns the folder of the deployments of BBjShip.
   *
   * @return the deployments folder
   */
  public Path getDeployments() {
    return runtime.resolve("deployments");
  }

  /**
   * Returns the file whose presence enables development runs.
   *
   * @return the development switch file
   */
  public Path getDevelopmentSwitch() {
    return runtime.resolve(DeploymentArchive.DEVELOPMENT_SWITCH);
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
   * Refuses a plugin folder that is a symbolic link.
   *
   * @throws IOException if the plugin folder is a symbolic link
   */
  void applyOwnershipValidation() throws IOException {
    if (Files.isSymbolicLink(directory)) {
      throw new IOException(
          "The plugin directory is a symbolic link. Install the plugin into a regular directory");
    }
  }
}
