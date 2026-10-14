package com.webforj.bbjship.foundation.model;

import java.nio.file.Path;
import java.util.Optional;

/**
 * Holds the hotswap options BBjServices must run with for development runs.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@SuppressWarnings("java:S6206")
public final class JavaSettings {

  private final Path propertiesFile;
  private final Path optionsDirectory;

  /**
   * Creates the Java settings.
   *
   * @param propertiesFile the properties file of BBjServices, null for the file in the cfg folder
   *        of the BBj home
   * @param optionsDirectory the directory holding the hotswap options file
   */
  public JavaSettings(Path propertiesFile, Path optionsDirectory) {
    this.propertiesFile = propertiesFile;
    this.optionsDirectory = optionsDirectory;
  }

  /**
   * Gets the properties file of BBjServices.
   *
   * @return the properties file, empty for the file in the cfg folder of the BBj home
   */
  public Optional<Path> getPropertiesFile() {
    return Optional.ofNullable(propertiesFile);
  }

  /**
   * Gets the directory holding the hotswap options file.
   *
   * @return the options directory
   */
  public Path getOptionsDirectory() {
    return optionsDirectory;
  }
}
