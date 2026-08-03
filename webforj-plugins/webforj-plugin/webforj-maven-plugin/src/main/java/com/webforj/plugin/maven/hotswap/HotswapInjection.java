package com.webforj.plugin.maven.hotswap;

import com.webforj.plugin.foundation.hotswap.HotswapAttachment;
import com.webforj.plugin.foundation.hotswap.HotswapTool;
import com.webforj.plugin.foundation.hotswap.jrebel.JrebelAttachment;
import com.webforj.plugin.maven.hotswap.jrebel.JrebelOptions;
import java.io.IOException;
import java.util.EnumSet;
import java.util.List;
import java.util.Optional;
import java.util.Properties;
import java.util.Set;
import java.util.function.Predicate;
import java.util.regex.Pattern;
import org.apache.maven.model.Plugin;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;

/**
 * Hands the configured hotswap agent to the application virtual machine.
 *
 * <p>
 * The watch goal runs in the build process before the application run goal on the same command
 * line, so the agent arguments are placed into the properties that goal reads for its fork. The
 * agent therefore enters only the application virtual machine, never the build process. For the
 * Jetty runner that requires the forked deploy mode, which is turned on when the build does not
 * pick a mode itself.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class HotswapInjection {

  /** The property that selects the tool on the command line. */
  public static final String SELECTION_PROPERTY = "webforj.hotswap";

  static final String SPRING_JVM_ARGUMENTS = "spring-boot.run.jvmArguments";
  static final String SPRING_RESTART_OFF = "-Dspring.devtools.restart.enabled=false";
  static final String JETTY_JVM_ARGS = "jetty.jvmArgs";
  static final String JETTY_DEPLOY_MODE = "jetty.deployMode";

  private static final String SPRING_PLUGIN = "spring-boot-maven-plugin";
  private static final Pattern JETTY_PLUGIN = Pattern.compile("jetty(-ee\\d+)?-maven-plugin");

  private final MavenProject project;
  private final Properties userProperties;
  private final HotswapOptions options;
  private final String commandLineValue;
  private final Log log;

  private HotswapInjection(Builder builder) {
    this.project = builder.project;
    this.userProperties = builder.userProperties;
    this.options = builder.options;
    this.commandLineValue = builder.commandLineValue;
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
   * Resolves the selected tool and places its arguments into the runner properties.
   *
   * @throws MojoExecutionException if the configuration is invalid or the agent cannot be resolved
   */
  public void apply() throws MojoExecutionException {
    Optional<HotswapTool> selected;
    try {
      selected = HotswapTool.select(configuredTools(), commandLineValue);
    } catch (IllegalArgumentException e) {
      throw new MojoExecutionException(e.getMessage(), e);
    }

    if (commandLineValue != null && !commandLineValue.isBlank()) {
      log.info("hotswap selection from the command line: " + commandLineValue);
    }

    if (selected.isEmpty()) {
      return;
    }

    inject(composeArguments(selected.get()));
  }

  private Set<HotswapTool> configuredTools() {
    Set<HotswapTool> configured = EnumSet.noneOf(HotswapTool.class);
    if (options != null && options.getJrebel() != null) {
      configured.add(HotswapTool.JREBEL);
    }

    return configured;
  }

  private List<String> composeArguments(HotswapTool tool) throws MojoExecutionException {
    try {
      return createAttachment(tool).arguments();
    } catch (IOException e) {
      throw new MojoExecutionException(e.getMessage(), e);
    }
  }

  private HotswapAttachment createAttachment(HotswapTool tool) throws MojoExecutionException {
    return switch (tool) {
      case JREBEL -> createJrebelAttachment();
    };
  }

  private HotswapAttachment createJrebelAttachment() throws MojoExecutionException {
    JrebelOptions jrebelOptions = options == null ? null : options.getJrebel();
    if (jrebelOptions == null || jrebelOptions.getPath() == null) {
      throw new MojoExecutionException(
          "set the jrebel path in the webforj plugin hotswap configuration, "
              + "the JRebel agent location cannot be guessed");
    }

    return JrebelAttachment.create().setPath(jrebelOptions.getPath().toPath()).setLog(log::info)
        .build();
  }

  private void inject(List<String> arguments) {
    boolean spring = hasPlugin(artifactId -> SPRING_PLUGIN.equals(artifactId));
    boolean jetty = hasPlugin(artifactId -> JETTY_PLUGIN.matcher(artifactId).matches());
    String joined = String.join(" ", arguments);

    if (spring) {
      // A development restart would replace the very classes the tool just swapped, so the two
      // must not run together, whichever tool is attached.
      appendProperty(SPRING_JVM_ARGUMENTS, joined + " " + SPRING_RESTART_OFF);
    }

    if (jetty) {
      injectIntoJetty(joined);
    }

    if (!spring && !jetty) {
      log.warn("hotswap is configured but the build has no supported application runner, "
          + "expected the Spring Boot plugin or the Jetty plugin");
    }
  }

  private void injectIntoJetty(String joined) {
    String mode = effectiveProperty(JETTY_DEPLOY_MODE);
    if (mode == null || mode.isBlank()) {
      project.getProperties().setProperty(JETTY_DEPLOY_MODE, "FORK");
    } else if (!"FORK".equalsIgnoreCase(mode)) {
      log.warn("hotswap needs the forked Jetty deploy mode, " + JETTY_DEPLOY_MODE + " is " + mode
          + ", the agent was not attached to the Jetty runner");

      return;
    }

    appendProperty(JETTY_JVM_ARGS, joined);
  }

  private String effectiveProperty(String key) {
    String fromCommandLine = userProperties.getProperty(key);

    return fromCommandLine != null ? fromCommandLine : project.getProperties().getProperty(key);
  }

  private boolean hasPlugin(Predicate<String> artifactId) {
    for (Plugin plugin : project.getBuildPlugins()) {
      if (artifactId.test(plugin.getArtifactId())) {
        return true;
      }
    }

    return false;
  }

  private void appendProperty(String key, String value) {
    String fromCommandLine = userProperties.getProperty(key);
    String existing =
        fromCommandLine != null ? fromCommandLine : project.getProperties().getProperty(key);
    String merged = existing == null || existing.isBlank() ? value : existing + " " + value;

    project.getProperties().setProperty(key, merged);
    // A value given on the command line outranks the project properties when the run goal reads
    // its parameters, so the merge must land there too or it would never be seen.
    if (fromCommandLine != null) {
      userProperties.setProperty(key, merged);
    }
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
