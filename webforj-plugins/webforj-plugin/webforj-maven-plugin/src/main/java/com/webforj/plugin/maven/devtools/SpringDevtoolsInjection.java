package com.webforj.plugin.maven.devtools;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import java.util.stream.Collectors;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.model.Plugin;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.eclipse.aether.RepositorySystem;
import org.eclipse.aether.RepositorySystemSession;
import org.eclipse.aether.artifact.DefaultArtifact;
import org.eclipse.aether.collection.CollectRequest;
import org.eclipse.aether.graph.Dependency;
import org.eclipse.aether.repository.RemoteRepository;
import org.eclipse.aether.resolution.ArtifactResult;
import org.eclipse.aether.resolution.DependencyRequest;
import org.eclipse.aether.resolution.DependencyResolutionException;
import org.eclipse.aether.util.artifact.JavaScopes;
import org.eclipse.aether.util.filter.DependencyFilterUtils;

/**
 * Hands the webforJ devtools JARs to the application run classpath.
 *
 * <p>
 * The watch goal runs in the build process before the application run goal on the same command
 * line, so the devtools JARs are placed into the extra classpath property that goal reads for its
 * fork. The devtools are resolved at the framework version found on the application classpath, and
 * only their own delta is added, the JARs that neither the application classpath nor the framework
 * dependency tree carries, so the application keeps the framework exactly as its build resolved it.
 * The run goal alone reads that property, so a packaged application can never contain the devtools.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class SpringDevtoolsInjection {

  static final String SPRING_ADDITIONAL_CLASSPATH = "spring-boot.run.additional-classpath-elements";
  static final String DEVTOOLS_GROUP_ID = "com.webforj";
  static final String DEVTOOLS_ARTIFACT_ID = "webforj-spring-devtools";

  private static final String SPRING_PLUGIN = "spring-boot-maven-plugin";
  private static final String FRAMEWORK_ARTIFACT_ID = "webforj-foundation";

  private final MavenProject project;
  private final Properties userProperties;
  private final Resolver resolver;
  private final Log log;

  private SpringDevtoolsInjection(Builder builder) {
    this.project = builder.project;
    this.userProperties = builder.userProperties;
    this.resolver = builder.resolver;
    this.log = builder.log;
  }

  /**
   * Creates a new builder for an injection.
   *
   * @return a new builder
   */
  public static Builder create() {
    return new Builder();
  }

  /**
   * Creates the resolver that resolves the devtools through the repositories of the build.
   *
   * @param system the repository system of the session
   * @param session the repository session of the build
   * @param repositories the remote repositories of the project
   * @return the resolver
   */
  public static Resolver resolver(RepositorySystem system, RepositorySystemSession session,
      List<RemoteRepository> repositories) {
    return (artifactId, version) -> {
      Dependency dependency =
          new Dependency(new DefaultArtifact(DEVTOOLS_GROUP_ID + ":" + artifactId + ":" + version),
              JavaScopes.RUNTIME);
      // The artifact enters the request as a dependency, not as the root, so its optional
      // dependencies are pruned the way every consumer resolves them.
      CollectRequest collect = new CollectRequest(List.of(dependency), null, repositories);
      DependencyRequest request =
          new DependencyRequest(collect, DependencyFilterUtils.classpathFilter(JavaScopes.RUNTIME));

      try {
        List<ResolvedJar> jars = new ArrayList<>();
        for (ArtifactResult resolved : system.resolveDependencies(session, request)
            .getArtifactResults()) {
          jars.add(new ResolvedJar(resolved.getArtifact().getGroupId(),
              resolved.getArtifact().getArtifactId(), resolved.getArtifact().getFile().toPath()));
        }

        return jars;
      } catch (DependencyResolutionException e) {
        throw new MojoExecutionException("could not resolve " + DEVTOOLS_GROUP_ID + ":" + artifactId
            + ":" + version + " for the application run classpath", e);
      }
    };
  }

  /**
   * Resolves the devtools JARs the application misses and places them into the runner property.
   *
   * @throws MojoExecutionException if the devtools cannot be resolved
   */
  public void apply() throws MojoExecutionException {
    if (!hasSpringPlugin()) {
      return;
    }

    String version = frameworkVersion();
    if (version == null) {
      log.debug("no webforJ framework on the application classpath, "
          + "the devtools delivery is skipped");

      return;
    }

    Set<String> present = project.getArtifacts().stream()
        .map(artifact -> key(artifact.getGroupId(), artifact.getArtifactId()))
        .collect(Collectors.toCollection(LinkedHashSet::new));

    // The framework tree also counts as present, whether or not the application graph carries all
    // of it, so the delivery adds only the devtools delta and never changes how the application
    // resolved the framework, for example a JAR its build deliberately excluded.
    for (ResolvedJar jar : resolver.resolve(FRAMEWORK_ARTIFACT_ID, version)) {
      present.add(key(jar.groupId(), jar.artifactId()));
    }

    List<Path> additions =
        new ArrayList<>(new LinkedHashSet<>(resolver.resolve(DEVTOOLS_ARTIFACT_ID, version).stream()
            .filter(jar -> !present.contains(key(jar.groupId(), jar.artifactId())))
            .map(ResolvedJar::file).toList()));

    if (additions.isEmpty()) {
      return;
    }

    log.info("devtools: handing " + additions.size()
        + " webforJ devtools JAR(s) to the application run classpath");
    appendProperty(SPRING_ADDITIONAL_CLASSPATH, additions.stream()
        .map(path -> path.toAbsolutePath().toString()).collect(Collectors.joining(",")));
  }

  private boolean hasSpringPlugin() {
    for (Plugin plugin : project.getBuildPlugins()) {
      if (SPRING_PLUGIN.equals(plugin.getArtifactId())) {
        return true;
      }
    }

    return false;
  }

  private String frameworkVersion() {
    for (Artifact artifact : project.getArtifacts()) {
      if (DEVTOOLS_GROUP_ID.equals(artifact.getGroupId())
          && FRAMEWORK_ARTIFACT_ID.equals(artifact.getArtifactId())) {
        return artifact.getBaseVersion();
      }
    }

    return null;
  }

  private static String key(String groupId, String artifactId) {
    return groupId + ":" + artifactId;
  }

  private void appendProperty(String key, String value) {
    String fromCommandLine = userProperties.getProperty(key);
    String existing =
        fromCommandLine != null ? fromCommandLine : project.getProperties().getProperty(key);
    String merged = existing == null || existing.isBlank() ? value : existing + "," + value;

    project.getProperties().setProperty(key, merged);
    // A value given on the command line outranks the project properties when the run goal reads
    // its parameters, so the merge must land there too or it would never be seen.
    if (fromCommandLine != null) {
      userProperties.setProperty(key, merged);
    }
  }

  /**
   * Resolves a webforJ dependency tree at a framework version.
   */
  @FunctionalInterface
  public interface Resolver {

    /**
     * Resolves the named webforJ artifact and its runtime dependencies at the given version.
     *
     * @param artifactId the webforJ artifact to resolve
     * @param version the framework version to resolve at
     * @return the resolved JARs
     * @throws MojoExecutionException if the resolution fails
     */
    List<ResolvedJar> resolve(String artifactId, String version) throws MojoExecutionException;
  }

  /**
   * One resolved JAR of the devtools dependency tree.
   *
   * @param groupId the group of the JAR
   * @param artifactId the artifact name of the JAR
   * @param file the JAR location on disk
   */
  public record ResolvedJar(String groupId, String artifactId, Path file) {}

  /**
   * Builds a {@link SpringDevtoolsInjection}.
   *
   * @author Hyyan Abo Fakher
   * @since 26.02
   */
  public static final class Builder {

    private MavenProject project;
    private Properties userProperties = new Properties();
    private Resolver resolver;
    private Log log;

    private Builder() {}

    /**
     * Sets the current Maven project.
     *
     * @param project the project
     * @return this builder
     */
    public Builder setProject(MavenProject project) {
      this.project = project;
      return this;
    }

    /**
     * Sets the command line properties of the session.
     *
     * @param userProperties the command line properties
     * @return this builder
     */
    public Builder setUserProperties(Properties userProperties) {
      this.userProperties = userProperties;
      return this;
    }

    /**
     * Sets the resolver the devtools dependency tree is resolved through.
     *
     * @param resolver the resolver
     * @return this builder
     */
    public Builder setResolver(Resolver resolver) {
      this.resolver = resolver;
      return this;
    }

    /**
     * Sets where progress is reported.
     *
     * @param log the goal log
     * @return this builder
     */
    public Builder setLog(Log log) {
      this.log = log;
      return this;
    }

    /**
     * Builds the injection.
     *
     * @return the injection
     */
    public SpringDevtoolsInjection build() {
      return new SpringDevtoolsInjection(this);
    }
  }
}
