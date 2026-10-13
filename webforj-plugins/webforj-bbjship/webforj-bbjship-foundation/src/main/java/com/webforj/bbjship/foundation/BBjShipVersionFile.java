package com.webforj.bbjship.foundation;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Properties;

/**
 * Reads the {@code version.properties} file of the BBjShip plugin.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class BBjShipVersionFile {

  private static final String FILE_NAME = "version.properties";

  private BBjShipVersionFile() {}

  /**
   * Reads the BBjShip version of a plugin folder.
   *
   * @param directory the plugin folder holding the version file
   *
   * @return the BBjShip version
   * @throws IOException if the version file cannot be read or names no version
   */
  public static String readVersion(Path directory) throws IOException {
    Properties properties = new Properties();
    try (InputStream input = Files.newInputStream(directory.resolve(FILE_NAME))) {
      properties.load(input);
    }

    String version = properties.getProperty("version", "");
    if (version.isBlank()) {
      throw new IOException("BBjShip version is missing in " + FILE_NAME);
    }

    return version;
  }
}
