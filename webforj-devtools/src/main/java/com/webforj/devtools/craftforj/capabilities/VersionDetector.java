package com.webforj.devtools.craftforj.capabilities;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

/**
 * Detects the craftforJ module version from build-time properties.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class VersionDetector {

  private static final String PROPERTIES_PATH = "craftforj.properties";
  private static final String VERSION_KEY = "version";

  private final String version;
  private final int major;
  private final int minor;
  private final boolean parsed;

  /**
   * Creates a new detector that reads from the default properties file.
   */
  VersionDetector() {
    this(loadVersionFromProperties());
  }

  /**
   * Creates a new detector with the given version string.
   *
   * @param version the version string (e.g. "25.12-SNAPSHOT"), or {@code null} if unknown
   */
  VersionDetector(String version) {
    this.version = version;

    int[] parts = parseVersion(version);
    if (parts.length == 2) {
      this.major = parts[0];
      this.minor = parts[1];
      this.parsed = true;
    } else {
      this.major = 0;
      this.minor = 0;
      this.parsed = false;
    }
  }

  /**
   * Gets the raw version string.
   *
   * @return the version string, or {@code null} if not found
   */
  String getVersion() {
    return version;
  }

  /**
   * Gets the version this craftforJ module was built at.
   *
   * <p>
   * craftforJ is built and released from the webforJ reactor, so this names the same release as the
   * webforJ on the classpath.
   * </p>
   *
   * @return the version string, or {@code null} if the build descriptor is missing
   */
  public static String moduleVersion() {
    return loadVersionFromProperties();
  }

  /**
   * Checks whether the version is at least the given major and minor.
   *
   * <p>
   * Returns {@code false} if the version could not be parsed.
   * </p>
   *
   * @param requiredMajor the minimum major version
   * @param requiredMinor the minimum minor version
   * @return {@code true} if the version meets the requirement
   */
  boolean isAtLeast(int requiredMajor, int requiredMinor) {
    if (!parsed) {
      return false;
    }

    if (major > requiredMajor) {
      return true;
    }

    return major == requiredMajor && minor >= requiredMinor;
  }

  /**
   * Parses a version string like "25.12-SNAPSHOT" into [major, minor].
   *
   * <p>
   * Strips any suffix after a hyphen before parsing.
   * </p>
   *
   * @param version the version string
   * @return an array of [major, minor], or an empty array if parsing fails
   */
  static int[] parseVersion(String version) {
    if (version == null || version.isEmpty()) {
      return new int[0];
    }

    // Strip qualifier (e.g. "-SNAPSHOT", "-RC1")
    String base = version;
    int hyphenIndex = base.indexOf('-');
    if (hyphenIndex > 0) {
      base = base.substring(0, hyphenIndex);
    }

    String[] parts = base.split("\\.");
    if (parts.length < 2) {
      return new int[0];
    }

    try {
      int major = Integer.parseInt(parts[0]);
      int minor = Integer.parseInt(parts[1]);
      return new int[] {major, minor};
    } catch (NumberFormatException e) {
      return new int[0];
    }
  }

  /**
   * Loads the version string from the properties file on the classpath.
   *
   * @return the version string, or {@code null} if not found
   */
  private static String loadVersionFromProperties() {
    try (InputStream is =
        VersionDetector.class.getClassLoader().getResourceAsStream(PROPERTIES_PATH)) {
      if (is == null) {
        return null;
      }

      Properties props = new Properties();
      props.load(is);
      String value = props.getProperty(VERSION_KEY);

      // If Maven filtering hasn't run, the value will still be the placeholder
      if (value != null && value.contains("${")) {
        return null;
      }

      return value;
    } catch (IOException e) {
      return null;
    }
  }
}
