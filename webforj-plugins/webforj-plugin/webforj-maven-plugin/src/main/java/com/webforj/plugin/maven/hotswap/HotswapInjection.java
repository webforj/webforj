package com.webforj.plugin.maven.hotswap;

import com.webforj.plugin.foundation.hotswap.HotswapLaunch;
import com.webforj.plugin.foundation.resolve.ArtifactResolver;
import com.webforj.plugin.maven.RunnerProperties;
import com.webforj.plugin.maven.hotswap.hotswapagent.HotswapAgentOptions;
import com.webforj.plugin.maven.hotswap.jrebel.JrebelOptions;
import com.webforj.plugin.maven.resolve.MavenArtifacts;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.Properties;
import java.util.function.Predicate;
import java.util.regex.Pattern;
import org.apache.maven.model.Plugin;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;

/**
 * Hands the configured hotswap tool to the application virtual machine of a Maven run.
 *
 * <p>
 * The watch goal runs in the build process before the application run goal on the same command
 * line, so the arguments the foundation launch composes are placed into the properties that goal
 * reads for its fork. The tool therefore enters only the application virtual machine, never the
 * build process. For the Jetty runner that requires the forked deploy mode, which is turned on when
 * the build does not pick a mode itself.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class HotswapInjection {

  static final String SPRING_JVM_ARGUMENTS = "spring-boot.run.jvmArguments";
  static final String JETTY_JVM_ARGS = "jetty.jvmArgs";
  static final String JETTY_DEPLOY_MODE = "jetty.deployMode";

  private static final String SPRING_PLUGIN = "spring-boot-maven-plugin";
  private static final Pattern JETTY_PLUGIN = Pattern.compile("jetty(-ee\\d+)?-maven-plugin");

  private final MavenProject project;
  private final Properties userProperties;
  private final HotswapOptions options;
  private final String commandLineValue;
  private final ArtifactResolver resolver;
  private final Log log;
  private final Path agentCacheRoot;
  private final Path javaExecutable;

  private HotswapInjection(Builder builder) {
    this.project = builder.project;
    this.userProperties = builder.userProperties;
    this.options = builder.options;
    this.commandLineValue = builder.commandLineValue;
    this.resolver = builder.resolver;
    this.log = builder.log;
    this.agentCacheRoot = builder.agentCacheRoot;
    this.javaExecutable = builder.javaExecutable;
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
   * Resolves the selected tool through the foundation launch and places its arguments into the
   * runner properties.
   *
   * @throws MojoExecutionException if the configuration is invalid or the launch cannot be prepared
   */
  public void apply() throws MojoExecutionException {
    List<String> arguments;
    try {
      arguments = createLaunch().getArguments();
    } catch (IllegalArgumentException | IOException e) {
      throw new MojoExecutionException(e.getMessage(), e);
    }

    if (arguments.isEmpty()) {
      return;
    }

    inject(String.join(" ", arguments));
  }

  private HotswapLaunch createLaunch() {
    HotswapAgentOptions agent = options == null ? null : options.getHotswapAgent();
    JrebelOptions jrebel = options == null ? null : options.getJrebel();
    String buildDirectory = project.getBuild() == null ? null : project.getBuild().getDirectory();

    return HotswapLaunch.create().setHotswapAgentConfigured(agent != null)
        .setHotswapAgentVersion(agent == null ? null : agent.getVersion())
        .setHotswapAgentPath(
            agent == null || agent.getPath() == null ? null : agent.getPath().toPath())
        .setJrebelConfigured(jrebel != null)
        .setJrebelPath(
            jrebel == null || jrebel.getPath() == null ? null : jrebel.getPath().toPath())
        .setCommandLineValue(commandLineValue)
        .setBuildDirectory(buildDirectory == null ? null : Path.of(buildDirectory))
        .setAgentCacheRoot(agentCacheRoot).setJavaExecutable(javaExecutable)
        .setApplicationClasspath(() -> MavenArtifacts.getApplicationClasspath(project))
        .setResolver(resolver).setLog(log::info).setWarn(log::warn).build();
  }

  private void inject(String joined) {
    boolean spring = hasPlugin(artifactId -> SPRING_PLUGIN.equals(artifactId));
    boolean jetty = hasPlugin(artifactId -> JETTY_PLUGIN.matcher(artifactId).matches());

    if (spring) {
      RunnerProperties.append(project, userProperties, SPRING_JVM_ARGUMENTS,
          joined + " " + HotswapLaunch.SPRING_RESTART_OFF, " ");
    }

    if (jetty) {
      injectIntoJetty(joined);
    }

    if (!spring && !jetty) {
      log.warn(HotswapLaunch.getMissingRunnerWarning("the Spring Boot plugin or the Jetty plugin"));
    }
  }

  private void injectIntoJetty(String joined) {
    String mode = RunnerProperties.getEffectiveValue(project, userProperties, JETTY_DEPLOY_MODE);
    if (mode == null || mode.isBlank()) {
      project.getProperties().setProperty(JETTY_DEPLOY_MODE, "FORK");
    } else if (!"FORK".equalsIgnoreCase(mode)) {
      log.warn("hotswap needs the forked Jetty deploy mode, " + JETTY_DEPLOY_MODE + " is " + mode
          + ", the agent was not attached to the Jetty runner");

      return;
    }

    RunnerProperties.append(project, userProperties, JETTY_JVM_ARGS, joined, " ");
  }

  private boolean hasPlugin(Predicate<String> artifactId) {
    for (Plugin plugin : project.getBuildPlugins()) {
      if (artifactId.test(plugin.getArtifactId())) {
        return true;
      }
    }

    return false;
  }

  /**
   * Builds a {@link HotswapInjection}.
   *
   * @author Hyyan Abo Fakher
   * @since 26.02
   */
  public static final class Builder {

    private MavenProject project;
    private Properties userProperties = new Properties();
    private HotswapOptions options;
    private String commandLineValue;
    private ArtifactResolver resolver;
    private Log log;
    private Path agentCacheRoot;
    private Path javaExecutable;

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
     * Sets the hotswap configuration, or null when not configured.
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
     * Sets the resolver the observer artifact is resolved through.
     *
     * @param resolver the resolver
     * @return this builder
     */
    public Builder setResolver(ArtifactResolver resolver) {
      this.resolver = resolver;
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
     * Sets the java executable the agent capability check runs, as named by the Maven toolchain.
     *
     * <p>
     * With null the check asks the running virtual machine directly, which is the virtual machine
     * the run goal forks when no toolchain is configured.
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
    public HotswapInjection build() {
      return new HotswapInjection(this);
    }
  }
}
