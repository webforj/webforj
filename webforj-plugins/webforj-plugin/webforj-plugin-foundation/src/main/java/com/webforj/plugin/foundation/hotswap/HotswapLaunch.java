package com.webforj.plugin.foundation.hotswap;

import com.webforj.plugin.foundation.hotswap.hotswapagent.HotswapAgentAttachment;
import com.webforj.plugin.foundation.hotswap.hotswapagent.HotswapObserverJar;
import com.webforj.plugin.foundation.hotswap.jrebel.JrebelAttachment;
import com.webforj.plugin.foundation.resolve.ApplicationClasspath;
import com.webforj.plugin.foundation.resolve.ArtifactResolver;
import java.io.IOException;
import java.nio.file.Path;
import java.util.EnumSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.function.Consumer;
import java.util.function.Supplier;

/**
 * Composes the virtual machine arguments of the configured hotswap tool.
 *
 * <p>
 * Everything both build plugins decide identically lives here: which tool the configuration and the
 * command line select, how each attachment is created, and every message those decisions produce. A
 * build plugin only maps its configuration language into this launch and places the returned
 * arguments into its own application runner.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class HotswapLaunch {

  /** The property that selects the tool on the command line. */
  public static final String SELECTION_PROPERTY = "webforj.hotswap";

  /**
   * Switches the Spring development restart off for the run. A development restart would replace
   * the very classes the tool just swapped, so the two must not run together, whichever tool is
   * attached. The runner adapter appends this wherever the Spring Boot runner is the target.
   */
  public static final String SPRING_RESTART_OFF = "-Dspring.devtools.restart.enabled=false";

  private final boolean hotswapAgentConfigured;
  private final String hotswapAgentVersion;
  private final Path hotswapAgentPath;
  private final boolean jrebelConfigured;
  private final Path jrebelPath;
  private final String commandLineValue;
  private final Path buildDirectory;
  private final Path agentCacheRoot;
  private final Path javaExecutable;
  private final Supplier<ApplicationClasspath> applicationClasspath;
  private final ArtifactResolver resolver;
  private final Consumer<String> log;
  private final Consumer<String> warn;

  private HotswapLaunch(Builder builder) {
    this.hotswapAgentConfigured = builder.hotswapAgentConfigured;
    this.hotswapAgentVersion = builder.hotswapAgentVersion;
    this.hotswapAgentPath = builder.hotswapAgentPath;
    this.jrebelConfigured = builder.jrebelConfigured;
    this.jrebelPath = builder.jrebelPath;
    this.commandLineValue = builder.commandLineValue;
    this.buildDirectory = builder.buildDirectory;
    this.agentCacheRoot = builder.agentCacheRoot != null ? builder.agentCacheRoot
        : Path.of(System.getProperty("user.home"), ".webforj", "hotswap-agent");
    this.javaExecutable = builder.javaExecutable;
    this.applicationClasspath = builder.applicationClasspath;
    this.resolver = builder.resolver;
    this.log = builder.log;
    this.warn = builder.warn;
  }

  /**
   * Creates a new builder for a launch.
   *
   * @return a new builder
   */
  public static Builder create() {
    return new Builder();
  }

  /**
   * The warning a build plugin reports when hotswap is configured without a supported application
   * runner.
   *
   * @param expectedRunners the runners the build plugin supports, named for the user
   * @return the warning line
   */
  public static String getMissingRunnerWarning(String expectedRunners) {
    return "hotswap is configured but the build has no supported application runner, expected "
        + expectedRunners;
  }

  /**
   * Resolves the selected tool and composes its virtual machine arguments.
   *
   * @return the arguments, one flag or value per element, empty when hotswap stays off
   * @throws IOException if the attachment cannot be prepared
   * @throws IllegalArgumentException if the configuration names more than one tool, the command
   *         line names no known tool, or the selected tool misses a required setting
   */
  public List<String> getArguments() throws IOException {
    Optional<HotswapTool> selected = HotswapTool.select(configuredTools(), commandLineValue);

    if (commandLineValue != null && !commandLineValue.isBlank()) {
      log.accept("hotswap selection from the command line: " + commandLineValue);
    }

    if (selected.isEmpty()) {
      return List.of();
    }

    return createAttachment(selected.get()).getArguments();
  }

  private Set<HotswapTool> configuredTools() {
    Set<HotswapTool> configured = EnumSet.noneOf(HotswapTool.class);
    if (hotswapAgentConfigured) {
      configured.add(HotswapTool.HOTSWAP_AGENT);
    }

    if (jrebelConfigured) {
      configured.add(HotswapTool.JREBEL);
    }

    return configured;
  }

  private HotswapAttachment createAttachment(HotswapTool tool) throws IOException {
    return switch (tool) {
      case HOTSWAP_AGENT -> createHotswapAgentAttachment();
      case JREBEL -> createJrebelAttachment();
    };
  }

  private HotswapAttachment createHotswapAgentAttachment() throws IOException {
    if (buildDirectory == null) {
      throw new IllegalArgumentException(
          "the hotswap agent needs the build directory of the project, none was supplied");
    }

    return HotswapAgentAttachment.create().setCacheRoot(agentCacheRoot)
        .setVersion(hotswapAgentVersion).setOverridePath(hotswapAgentPath)
        .setObserverJar(HotswapObserverJar.resolve(applicationClasspath.get(), resolver))
        .setConfigurationDirectory(buildDirectory.resolve("hotswap"))
        .setJavaExecutable(javaExecutable).setLog(log).setWarn(warn).build();
  }

  private HotswapAttachment createJrebelAttachment() {
    if (jrebelPath == null) {
      throw new IllegalArgumentException("set the jrebel path in the webforJ hotswap "
          + "configuration, the JRebel agent location cannot be guessed");
    }

    return JrebelAttachment.create().setPath(jrebelPath).setLog(log).build();
  }

  /**
   * Builds a {@link HotswapLaunch}.
   *
   * @author Hyyan Abo Fakher
   * @since 26.02
   */
  public static final class Builder {

    private boolean hotswapAgentConfigured;
    private String hotswapAgentVersion;
    private Path hotswapAgentPath;
    private boolean jrebelConfigured;
    private Path jrebelPath;
    private String commandLineValue;
    private Path buildDirectory;
    private Path agentCacheRoot;
    private Path javaExecutable;
    private Supplier<ApplicationClasspath> applicationClasspath;
    private ArtifactResolver resolver;
    private Consumer<String> log = line -> {
    };
    private Consumer<String> warn = line -> {
    };

    private Builder() {}

    /**
     * Sets whether the build configured the HotswapAgent tool.
     *
     * @param hotswapAgentConfigured true when HotswapAgent was configured
     * @return this builder
     */
    public Builder setHotswapAgentConfigured(boolean hotswapAgentConfigured) {
      this.hotswapAgentConfigured = hotswapAgentConfigured;
      return this;
    }

    /**
     * Sets the HotswapAgent version to download, or null for the default.
     *
     * @param hotswapAgentVersion the pinned version
     * @return this builder
     */
    public Builder setHotswapAgentVersion(String hotswapAgentVersion) {
      this.hotswapAgentVersion = hotswapAgentVersion;
      return this;
    }

    /**
     * Sets the HotswapAgent jar already on disk, or null to download.
     *
     * @param hotswapAgentPath the agent jar
     * @return this builder
     */
    public Builder setHotswapAgentPath(Path hotswapAgentPath) {
      this.hotswapAgentPath = hotswapAgentPath;
      return this;
    }

    /**
     * Sets whether the build configured the JRebel tool.
     *
     * @param jrebelConfigured true when JRebel was configured
     * @return this builder
     */
    public Builder setJrebelConfigured(boolean jrebelConfigured) {
      this.jrebelConfigured = jrebelConfigured;
      return this;
    }

    /**
     * Sets the JRebel agent on disk, a native library or a jar.
     *
     * @param jrebelPath the agent path
     * @return this builder
     */
    public Builder setJrebelPath(Path jrebelPath) {
      this.jrebelPath = jrebelPath;
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
     * Sets the cache root the downloaded agent jars live under, or null for the default under the
     * user home.
     *
     * @param agentCacheRoot the cache root
     * @return this builder
     */
    public Builder setAgentCacheRoot(Path agentCacheRoot) {
      this.agentCacheRoot = agentCacheRoot;
      return this;
    }

    /**
     * Sets the java executable of the virtual machine the application runs in, as named by the
     * build system, or null when the application runs in the current one.
     *
     * @param javaExecutable the java executable
     * @return this builder
     */
    public Builder setJavaExecutable(Path javaExecutable) {
      this.javaExecutable = javaExecutable;
      return this;
    }

    /**
     * Sets the resolved runtime classpath of the application, asked only when the selected tool
     * needs it.
     *
     * @param applicationClasspath supplies the application classpath
     * @return this builder
     */
    public Builder setApplicationClasspath(Supplier<ApplicationClasspath> applicationClasspath) {
      this.applicationClasspath = applicationClasspath;
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
     * Sets where warnings are reported.
     *
     * @param warn the warning sink
     * @return this builder
     */
    public Builder setWarn(Consumer<String> warn) {
      this.warn = warn != null ? warn : line -> {
      };
      return this;
    }

    /**
     * Builds the launch.
     *
     * @return the launch
     */
    public HotswapLaunch build() {
      return new HotswapLaunch(this);
    }
  }
}
