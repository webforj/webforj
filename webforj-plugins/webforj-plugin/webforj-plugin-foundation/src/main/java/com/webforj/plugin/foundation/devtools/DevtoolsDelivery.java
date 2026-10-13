package com.webforj.plugin.foundation.devtools;

import com.webforj.bbjship.foundation.ApplicationClasspath;
import com.webforj.bbjship.foundation.ApplicationClasspath.ResolvedJar;
import com.webforj.plugin.foundation.resolve.ArtifactResolver;
import java.io.IOException;
import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Consumer;

/**
 * Computes the webforJ devtools jars an application run classpath misses.
 *
 * <p>
 * The devtools are resolved at the framework version found on the application classpath, and only
 * their own delta is delivered, the jars that neither the application classpath nor the framework
 * dependency tree carries, so the application keeps the framework exactly as its build resolved it.
 * Each build plugin maps its dependency world into the neutral inputs and places the returned jars
 * onto its own run classpath, the decision logic lives only here.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class DevtoolsDelivery {

  private final String artifactId;
  private final ApplicationClasspath applicationClasspath;
  private final ArtifactResolver resolver;
  private final Consumer<String> log;
  private final Consumer<String> debug;

  private DevtoolsDelivery(Builder builder) {
    this.artifactId = builder.artifactId;
    this.applicationClasspath = builder.applicationClasspath;
    this.resolver = builder.resolver;
    this.log = builder.log;
    this.debug = builder.debug;
  }

  /**
   * Creates a new builder for a delivery.
   *
   * @return a new builder
   */
  public static Builder create() {
    return new Builder();
  }

  /**
   * Resolves the devtools jars the application misses.
   *
   * @return the missing jars, empty when the application carries everything or has no webforJ
   *         framework on its classpath
   * @throws IOException if the resolution fails
   */
  public List<ResolvedJar> getJars() throws IOException {
    Optional<String> version = applicationClasspath.getFrameworkVersion();
    if (version.isEmpty()) {
      debug.accept(
          "no webforJ framework on the application classpath, the devtools delivery is skipped");

      return List.of();
    }

    // The framework tree also counts as present, whether or not the application graph carries all
    // of it, so the delivery adds only the devtools delta and never changes how the application
    // resolved the framework, for example a jar its build deliberately excluded.
    Set<String> present = applicationClasspath.getModuleKeys();
    for (ResolvedJar jar : resolver.resolve(ApplicationClasspath.FRAMEWORK_GROUP_ID,
        ApplicationClasspath.FRAMEWORK_ARTIFACT_ID, version.get())) {
      present.add(ApplicationClasspath.getModuleKey(jar));
    }

    Map<Path, ResolvedJar> additions = new LinkedHashMap<>();
    for (ResolvedJar jar : resolver.resolve(ApplicationClasspath.FRAMEWORK_GROUP_ID, artifactId,
        version.get())) {
      if (!present.contains(ApplicationClasspath.getModuleKey(jar))) {
        additions.putIfAbsent(jar.file(), jar);
      }
    }

    if (!additions.isEmpty()) {
      log.accept("devtools: handing " + additions.size()
          + " webforJ devtools JAR(s) to the application run classpath");
    }

    return List.copyOf(additions.values());
  }

  /**
   * Builds a {@link DevtoolsDelivery}.
   *
   * @author Hyyan Abo Fakher
   * @since 26.02
   */
  public static final class Builder {

    private String artifactId;
    private ApplicationClasspath applicationClasspath;
    private ArtifactResolver resolver;
    private Consumer<String> log = line -> {
    };
    private Consumer<String> debug = line -> {
    };

    private Builder() {}

    /**
     * Sets the devtools artifact delivered at the framework version.
     *
     * @param artifactId the artifact id
     * @return this builder
     */
    public Builder setArtifactId(String artifactId) {
      this.artifactId = artifactId;
      return this;
    }

    /**
     * Sets the resolved runtime classpath of the application.
     *
     * @param applicationClasspath the application classpath
     * @return this builder
     */
    public Builder setApplicationClasspath(ApplicationClasspath applicationClasspath) {
      this.applicationClasspath = applicationClasspath;
      return this;
    }

    /**
     * Sets the resolver the webforJ dependency trees are resolved through.
     *
     * @param resolver the resolver
     * @return this builder
     */
    public Builder setResolver(ArtifactResolver resolver) {
      this.resolver = resolver;
      return this;
    }

    /**
     * Sets where progress lines are reported.
     *
     * @param log the log sink
     * @return this builder
     */
    public Builder setLog(Consumer<String> log) {
      this.log = log != null ? log : line -> {
      };
      return this;
    }

    /**
     * Sets where diagnostic lines are reported.
     *
     * @param debug the diagnostic sink
     * @return this builder
     */
    public Builder setDebug(Consumer<String> debug) {
      this.debug = debug != null ? debug : line -> {
      };
      return this;
    }

    /**
     * Builds the delivery.
     *
     * @return the delivery
     * @throws IllegalStateException if the artifact id is missing
     */
    public DevtoolsDelivery build() {
      if (artifactId == null || artifactId.isBlank()) {
        throw new IllegalStateException("the devtools artifact id is required");
      }

      return new DevtoolsDelivery(this);
    }
  }
}
