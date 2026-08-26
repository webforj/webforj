package com.webforj.push;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

/**
 * Reads the version of this module, which the worker URL carries so every upgrade of the module
 * installs a fresh worker.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
final class PushVersion {

  static final String PROPERTIES_PATH = "webforj-push.properties";
  static final String UNKNOWN = "unknown";
  private static final String VERSION_KEY = "version";
  private static final String VERSION = load();

  private PushVersion() {}

  /**
   * Returns the module version.
   *
   * @return the version, {@link #UNKNOWN} when the build did not stamp one
   */
  static String get() {
    return VERSION;
  }

  static String read(InputStream stream) throws IOException {
    if (stream == null) {
      return UNKNOWN;
    }

    Properties properties = new Properties();
    properties.load(stream);
    String value = properties.getProperty(VERSION_KEY);

    if (value == null || value.isBlank() || value.contains("${")) {
      return UNKNOWN;
    }

    return value.trim();
  }

  private static String load() {
    try (InputStream stream =
        PushVersion.class.getClassLoader().getResourceAsStream(PROPERTIES_PATH)) {
      return read(stream);
    } catch (IOException e) {
      return UNKNOWN;
    }
  }
}
