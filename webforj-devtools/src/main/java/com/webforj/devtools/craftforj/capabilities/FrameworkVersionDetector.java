package com.webforj.devtools.craftforj.capabilities;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

/**
 * Detects the webforJ framework version present on the classpath.
 *
 * <p>
 * Version-scoped capabilities gate on the framework actually running, not on the craftforJ build,
 * so a craftforJ release paired with an older framework never offers a feature that framework
 * cannot support. The version is read from the webforJ core jar's Maven descriptor. In a reactor or
 * IDE run that descriptor is absent and the craftforJ module version stands in, since the two are
 * the same release there by construction.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class FrameworkVersionDetector {

  private static final String POM_PROPERTIES_PATH =
      "META-INF/maven/com.webforj/webforj/pom.properties";
  private static final String VERSION_KEY = "version";

  private final VersionDetector version;

  /**
   * Creates a detector that resolves the framework version from the classpath.
   */
  FrameworkVersionDetector() {
    this(resolveVersion());
  }

  /**
   * Creates a detector for a known version string.
   *
   * @param version the framework version (e.g. "26.02-SNAPSHOT"), or {@code null} if unknown
   */
  FrameworkVersionDetector(String version) {
    this.version = new VersionDetector(version);
  }

  /**
   * Gets the resolved framework version string.
   *
   * @return the version string, or {@code null} if it could not be resolved
   */
  String getVersion() {
    return version.getVersion();
  }

  /**
   * Checks whether the framework version is at least the given major and minor.
   *
   * @param requiredMajor the minimum major version
   * @param requiredMinor the minimum minor version
   * @return {@code true} if the framework meets the requirement
   */
  boolean isAtLeast(int requiredMajor, int requiredMinor) {
    return version.isAtLeast(requiredMajor, requiredMinor);
  }

  /**
   * Resolves the framework version from the core jar's Maven descriptor, falling back to the
   * craftforJ module version when the descriptor is absent.
   *
   * @return the resolved version string, or {@code null} if neither source is available
   */
  private static String resolveVersion() {
    String fromClasspath = loadFromPomProperties();
    return fromClasspath != null ? fromClasspath : VersionDetector.moduleVersion();
  }

  /**
   * Reads the version from the webforJ core jar's embedded Maven descriptor on the classpath.
   *
   * @return the version string, or {@code null} if the descriptor is missing or unreadable
   */
  private static String loadFromPomProperties() {
    try (InputStream is =
        FrameworkVersionDetector.class.getClassLoader().getResourceAsStream(POM_PROPERTIES_PATH)) {
      if (is == null) {
        return null;
      }

      Properties props = new Properties();
      props.load(is);

      return props.getProperty(VERSION_KEY);
    } catch (IOException e) {
      return null;
    }
  }
}
