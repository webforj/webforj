package com.webforj.plugin.gradle.hotswap;

import com.webforj.plugin.foundation.hotswap.HotswapLaunch;
import com.webforj.plugin.foundation.resolve.ApplicationClasspath;
import com.webforj.plugin.foundation.resolve.ArtifactResolver;
import com.webforj.plugin.gradle.resolve.GradleArtifacts;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Supplier;
import org.gradle.api.GradleException;
import org.gradle.api.Project;
import org.gradle.api.logging.Logger;

/**
 * Hands the configured hotswap tool to the application virtual machine of a Gradle run.
 *
 * <p>
 * The arguments the foundation launch composes are asked for when the application run task starts,
 * so nothing is resolved for a build that never launches the application.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class HotswapInjection {

  private final HotswapOptions options;
  private final String commandLineValue;
  private final boolean springBootRunner;
  private final Path buildDirectory;
  private final Path javaExecutable;
  private final Logger log;
  private final Path agentCacheRoot;
  private final Supplier<ApplicationClasspath> applicationClasspath;
  private final ArtifactResolver resolver;

  private HotswapInjection(Builder builder) {
    this.options = builder.options;
    this.commandLineValue = builder.commandLineValue;
    this.springBootRunner = builder.springBootRunner;
    this.buildDirectory = builder.buildDirectory;
    this.javaExecutable = builder.javaExecutable;
    this.log = builder.log;
    this.agentCacheRoot = builder.agentCacheRoot;

    Project project = builder.project;
    this.applicationClasspath = builder.applicationClasspath != null ? builder.applicationClasspath
        : () -> GradleArtifacts.getApplicationClasspath(project);
    this.resolver =
        builder.resolver != null ? builder.resolver : GradleArtifacts.getResolver(project);
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
   * Resolves the selected tool through the foundation launch and composes its virtual machine
   * arguments.
   *
   * @return the arguments, empty when hotswap stays off
   */
  public List<String> getArguments() {
    List<String> composed;
    try {
      composed = createLaunch().getArguments();
    } catch (IllegalArgumentException | IOException e) {
      throw new GradleException(e.getMessage(), e);
    }

    if (composed.isEmpty() || !springBootRunner) {
      return composed;
    }

    List<String> arguments = new ArrayList<>(composed);
    arguments.add(HotswapLaunch.SPRING_RESTART_OFF);

    return List.copyOf(arguments);
  }

  private HotswapLaunch createLaunch() {
    return HotswapLaunch.create().setHotswapAgentConfigured(options.isHotswapAgentConfigured())
        .setHotswapAgentVersion(options.getHotswapAgent().getVersion().getOrNull())
        .setHotswapAgentPath(options.getHotswapAgent().getPath().isPresent()
            ? options.getHotswapAgent().getPath().get().getAsFile().toPath()
            : null)
        .setJrebelConfigured(options.isJrebelConfigured())
        .setJrebelPath(options.getJrebel().getPath().isPresent()
            ? options.getJrebel().getPath().get().getAsFile().toPath()
            : null)
        .setCommandLineValue(commandLineValue).setBuildDirectory(buildDirectory)
        .setAgentCacheRoot(agentCacheRoot).setJavaExecutable(javaExecutable)
        .setApplicationClasspath(applicationClasspath).setResolver(resolver).setLog(log::lifecycle)
        .setWarn(log::warn).build();
  }

  /**
   * Builds a {@link HotswapInjection}.
   *
   * @author Hyyan Abo Fakher
   * @since 26.02
   */
  public static final class Builder {

    private Project project;
    private HotswapOptions options;
    private String commandLineValue;
    private boolean springBootRunner;
    private Path buildDirectory;
    private Path javaExecutable;
    private Logger log;
    private Path agentCacheRoot;
    private Supplier<ApplicationClasspath> applicationClasspath;
    private ArtifactResolver resolver;

    private Builder() {}

    /**
     * Sets the project the application runs from.
     *
     * @param project the project
     * @return this builder
     */
    public Builder setProject(Project project) {
      this.project = project;
      return this;
    }

    /**
     * Sets the hotswap configuration.
     *
     * @param options the hotswap configuration
     * @return this builder
     */
    public Builder setOptions(HotswapOptions options) {
      this.options = options;
      return this;
    }

    /**
     * Sets the value of the selection property, or null when it was not given.
     *
     * @param commandLineValue the selection value
     * @return this builder
     */
    public Builder setCommandLineValue(String commandLineValue) {
      this.commandLineValue = commandLineValue;
      return this;
    }

    /**
     * Sets whether the arguments go to the Spring Boot run task.
     *
     * @param springBootRunner true for the Spring Boot run task
     * @return this builder
     */
    public Builder setSpringBootRunner(boolean springBootRunner) {
      this.springBootRunner = springBootRunner;
      return this;
    }

    /**
     * Sets the build directory the generated agent configuration is written into.
     *
     * @param buildDirectory the build directory
     * @return this builder
     */
    public Builder setBuildDirectory(Path buildDirectory) {
      this.buildDirectory = buildDirectory;
      return this;
    }

    /**
     * Sets the java executable of the virtual machine the run task forks, as named by the Gradle
     * toolchain.
     *
     * <p>
     * With null the check asks the running virtual machine directly, which is the virtual machine
     * the application runs in when no toolchain is configured.
     * </p>
     *
     * @param javaExecutable the java executable, or null to ask the running virtual machine
     * @return this builder
     */
    public Builder setJavaExecutable(Path javaExecutable) {
      this.javaExecutable = javaExecutable;
      return this;
    }

    /**
     * Sets where progress is reported.
     *
     * @param log the project logger
     * @return this builder
     */
    public Builder setLog(Logger log) {
      this.log = log;
      return this;
    }

    /**
     * Sets the cache root the downloaded agent jars live under, or null for the default under the
     * user home.
     *
     * @param agentCacheRoot the cache root
     * @return this builder
     */
    Builder setAgentCacheRoot(Path agentCacheRoot) {
      this.agentCacheRoot = agentCacheRoot;
      return this;
    }

    /**
     * Sets the application classpath the launch composes against, or null to resolve it from the
     * project.
     *
     * @param applicationClasspath the application classpath
     * @return this builder
     */
    Builder setApplicationClasspath(Supplier<ApplicationClasspath> applicationClasspath) {
      this.applicationClasspath = applicationClasspath;
      return this;
    }

    /**
     * Sets the resolver the observer artifact is resolved through, or null to resolve through the
     * repositories of the build.
     *
     * @param resolver the resolver
     * @return this builder
     */
    Builder setResolver(ArtifactResolver resolver) {
      this.resolver = resolver;
      return this;
    }

    /**
     * Builds the injection.
     *
     * @return the injection
     */
    public HotswapInjection build() {
      return new HotswapInjection(this);
    }
  }
}
