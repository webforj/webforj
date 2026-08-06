package com.webforj.plugin.foundation.resolve;

import java.nio.file.Path;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

/**
 * The resolved runtime classpath of the application, as the build resolved it.
 *
 * <p>
 * The classpath answers the questions every delivery asks before touching the application: which
 * webforJ framework version the application runs, and which modules it already carries.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class ApplicationClasspath {

  /** The group of the webforJ framework artifact. */
  public static final String FRAMEWORK_GROUP_ID = "com.webforj";

  /** The artifact name the webforJ framework is recognized by on the classpath. */
  public static final String FRAMEWORK_ARTIFACT_ID = "webforj-foundation";

  private final List<ResolvedJar> jars;

  /**
   * Creates the classpath from the resolved jars of the application.
   *
   * @param jars the resolved jars
   */
  public ApplicationClasspath(List<ResolvedJar> jars) {
    this.jars = List.copyOf(jars);
  }

  /**
   * The webforJ framework version found on this classpath.
   *
   * @return the framework version, or empty when the application carries no webforJ framework
   */
  public Optional<String> getFrameworkVersion() {
    for (ResolvedJar jar : jars) {
      if (FRAMEWORK_GROUP_ID.equals(jar.groupId())
          && FRAMEWORK_ARTIFACT_ID.equals(jar.artifactId())) {
        return Optional.of(jar.version());
      }
    }

    return Optional.empty();
  }

  /**
   * The module keys of every jar on this classpath.
   *
   * @return the module keys, in classpath order
   */
  public Set<String> getModuleKeys() {
    Set<String> keys = new LinkedHashSet<>();
    for (ResolvedJar jar : jars) {
      keys.add(getModuleKey(jar));
    }

    return keys;
  }

  /**
   * The module key of a jar, the identity the delta computations compare by.
   *
   * @param jar the jar to name
   * @return the group and artifact name joined by a colon
   */
  public static String getModuleKey(ResolvedJar jar) {
    return jar.groupId() + ":" + jar.artifactId();
  }

  /**
   * One jar of a resolved dependency tree, named by its module coordinates.
   *
   * @param groupId the group of the jar
   * @param artifactId the artifact name of the jar
   * @param version the version of the jar
   * @param file the jar location on disk, or null when the entry only names a module of the
   *        application classpath
   */
  public record ResolvedJar(String groupId, String artifactId, String version, Path file) {}
}
