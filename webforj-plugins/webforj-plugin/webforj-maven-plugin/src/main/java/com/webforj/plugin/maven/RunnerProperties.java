package com.webforj.plugin.maven;

import java.util.Properties;
import org.apache.maven.project.MavenProject;

/**
 * Merges values into the properties the application run goals read for their forks.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class RunnerProperties {

  private RunnerProperties() {}

  /**
   * Appends a value to a runner property, keeping whatever the build supplied first.
   *
   * @param project the current Maven project
   * @param userProperties the command line properties of the session
   * @param key the property the run goal reads
   * @param value the value to append
   * @param separator the separator the run goal splits the property by
   */
  public static void append(MavenProject project, Properties userProperties, String key,
      String value, String separator) {
    String fromCommandLine = userProperties.getProperty(key);
    String existing =
        fromCommandLine != null ? fromCommandLine : project.getProperties().getProperty(key);
    String merged = existing == null || existing.isBlank() ? value : existing + separator + value;

    project.getProperties().setProperty(key, merged);
    // A value given on the command line outranks the project properties when the run goal reads
    // its parameters, so the merge must land there too or it would never be seen.
    if (fromCommandLine != null) {
      userProperties.setProperty(key, merged);
    }
  }

  /**
   * The value of a runner property as the run goal will see it.
   *
   * @param project the current Maven project
   * @param userProperties the command line properties of the session
   * @param key the property the run goal reads
   *
   * @return the effective value, or null when the property is not set
   */
  public static String getEffectiveValue(MavenProject project, Properties userProperties,
      String key) {
    String fromCommandLine = userProperties.getProperty(key);

    return fromCommandLine != null ? fromCommandLine : project.getProperties().getProperty(key);
  }
}
