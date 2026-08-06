package com.webforj.plugin.maven.devtools;

import com.webforj.plugin.foundation.devtools.SpringDevtoolsDelivery;
import com.webforj.plugin.foundation.resolve.ArtifactResolver;
import com.webforj.plugin.maven.RunnerProperties;
import com.webforj.plugin.maven.resolve.MavenArtifacts;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.Properties;
import java.util.stream.Collectors;
import org.apache.maven.model.Plugin;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;

/**
 * Hands the webforJ devtools jars to the application run classpath of a Maven run.
 *
 * <p>
 * The watch goal runs in the build process before the application run goal on the same command
 * line, so the jars the foundation delivery computes are placed into the extra classpath property
 * that goal reads for its fork. The run goal alone reads that property, so a packaged application
 * can never contain the devtools.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class SpringDevtoolsInjection {

  static final String SPRING_ADDITIONAL_CLASSPATH = "spring-boot.run.additional-classpath-elements";

  private static final String SPRING_PLUGIN = "spring-boot-maven-plugin";

  private final MavenProject project;
  private final Properties userProperties;
  private final ArtifactResolver resolver;
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
   * Computes the devtools jars the application misses and places them into the runner property.
   *
   * @throws MojoExecutionException if the devtools cannot be resolved
   */
  public void apply() throws MojoExecutionException {
    if (!hasSpringPlugin()) {
      return;
    }

    List<Path> additions;
    try {
      additions = SpringDevtoolsDelivery.create()
          .setApplicationClasspath(MavenArtifacts.getApplicationClasspath(project))
          .setResolver(resolver).setLog(log::info).setDebug(log::debug).build().getJars();
    } catch (IOException e) {
      throw new MojoExecutionException(e.getMessage(), e);
    }

    if (additions.isEmpty()) {
      return;
    }

    RunnerProperties.append(project, userProperties, SPRING_ADDITIONAL_CLASSPATH, additions.stream()
        .map(path -> path.toAbsolutePath().toString()).collect(Collectors.joining(",")), ",");
  }

  private boolean hasSpringPlugin() {
    for (Plugin plugin : project.getBuildPlugins()) {
      if (SPRING_PLUGIN.equals(plugin.getArtifactId())) {
        return true;
      }
    }

    return false;
  }

  /**
   * Builds a {@link SpringDevtoolsInjection}.
   *
   * @author Hyyan Abo Fakher
   * @since 26.02
   */
  public static final class Builder {

    private MavenProject project;
    private Properties userProperties = new Properties();
    private ArtifactResolver resolver;
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
    public Builder setResolver(ArtifactResolver resolver) {
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
