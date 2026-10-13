package com.webforj.plugin.foundation.bbj;

import com.webforj.plugin.foundation.bbj.model.BBjCredentials;
import com.webforj.plugin.foundation.bbj.model.JavaSettings;
import com.webforj.plugin.foundation.devtools.DevtoolsDelivery;
import com.webforj.plugin.foundation.resolve.ApplicationClasspath;
import com.webforj.plugin.foundation.resolve.ApplicationClasspath.ResolvedJar;
import com.webforj.plugin.foundation.resolve.ArtifactResolver;
import java.io.IOException;
import java.net.URI;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.function.Consumer;

/**
 * Starts and stops the development run of an application on the BBjShip of the local BBj
 * installation.
 *
 * <p>
 * A development run publishes the application from its class folders with the runtime dependencies
 * and the webforJ devtools the application lacks. Starting it sets up the local BBjShip for
 * development runs first.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class Development {

  /**
   * The message a build reports when a development run cannot be started.
   */
  public static final String FAILURE = "BBj development run failed. Check the deployment "
      + "settings and that development runs are enabled on the BBj machine";

  private static final String DEVTOOLS_ARTIFACT_ID = "webforj-devtools";
  private static final String RUN_PREFIX = "webforJ watch: development run of ";

  private final Path bundle;
  private final String version;
  private final URI deploymentUrl;
  private final String publishName;
  private final BBjCredentials credentials;
  private final Path home;
  private final JavaSettings javaSettings;
  private final Path archive;
  private final List<Path> classFolders;
  private final ApplicationClasspath applicationClasspath;
  private final ApplicationClasspath runtimeClasspath;
  private final ArtifactResolver resolver;
  private final Consumer<String> log;
  private final Consumer<String> warn;
  private final Consumer<String> debug;
  private BBjShipClient bbjShip;

  private Development(Builder builder) {
    this.bundle = builder.bundle;
    this.version = builder.version;
    this.deploymentUrl = builder.deploymentUrl;
    this.publishName = builder.publishName;
    this.credentials = builder.credentials;
    this.home = builder.home;
    this.javaSettings = builder.javaSettings;
    this.archive = builder.archive;
    this.classFolders = builder.classFolders;
    this.applicationClasspath = builder.applicationClasspath;
    this.runtimeClasspath = builder.runtimeClasspath;
    this.resolver = builder.resolver;
    this.log = builder.log;
    this.warn = builder.warn;
    this.debug = builder.debug;
  }

  /**
   * Creates a new builder for the development run of a BBjShip package.
   *
   * @param bundle the BBjShip ZIP the setup installs
   * @param version the webforJ version BBjShip must match
   *
   * @return a new builder
   * @throws NullPointerException if an argument is null
   */
  public static Builder create(Path bundle, String version) {
    return new Builder(bundle, version);
  }

  /**
   * Sets up the local BBjShip for development runs and deploys the development run.
   *
   * @return the address of the running application
   * @throws IOException if the setup, the devtools resolution, the archive or the deployment fails
   * @throws IllegalArgumentException if the deployment URL is no valid BBjShip address or no class
   *         folder is set
   */
  public URI start() throws IOException {
    Setup.Builder setup = Setup.create(bundle, version).setDeploymentUrl(deploymentUrl)
        .setCredentials(credentials).setPort(BBjShipClient.getPort(deploymentUrl))
        .setDevelopmentEnabled(true).setLog(log).setWarn(warn);
    if (home != null) {
      setup.setHome(home);
    }

    if (javaSettings != null) {
      setup.setJavaSettings(javaSettings);
    }

    setup.build().apply();

    List<ResolvedJar> jars = new ArrayList<>(runtimeClasspath.getJars());
    jars.addAll(DevtoolsDelivery.create().setArtifactId(DEVTOOLS_ARTIFACT_ID)
        .setApplicationClasspath(applicationClasspath).setResolver(resolver).setLog(log)
        .setDebug(debug).build().getJars());
    Deployment.create(archive).setClasspath(new ApplicationClasspath(jars))
        .setClassFolders(classFolders).build().writeArchive();
    bbjShip = BBjShipClient.create(deploymentUrl).setCredentials(credentials).setVersion(version)
        .setLog(log).setWarn(warn).build();

    return bbjShip.deploy(archive, publishName);
  }

  /**
   * Removes the started development run from BBjShip.
   *
   * <p>
   * A failure is reported as a warning.
   * </p>
   */
  public void stop() {
    if (bbjShip == null) {
      return;
    }

    try {
      bbjShip.undeploy(publishName);
      log.accept(RUN_PREFIX + publishName + " stopped");
    } catch (IOException failure) {
      warn.accept(RUN_PREFIX + publishName + " was not stopped. " + failure.getMessage());
    }
  }

  /**
   * Builds a {@link Development}.
   *
   * @author Hyyan Abo Fakher
   * @since 26.03
   */
  public static final class Builder {

    private final Path bundle;
    private final String version;
    private URI deploymentUrl;
    private String publishName;
    private BBjCredentials credentials;
    private Path home;
    private JavaSettings javaSettings;
    private Path archive;
    private List<Path> classFolders;
    private ApplicationClasspath applicationClasspath;
    private ApplicationClasspath runtimeClasspath;
    private ArtifactResolver resolver;
    private Consumer<String> log = line -> {
    };
    private Consumer<String> warn = line -> {
    };
    private Consumer<String> debug = line -> {
    };

    private Builder(Path bundle, String version) {
      this.bundle = Objects.requireNonNull(bundle);
      this.version = Objects.requireNonNull(version);
    }

    /**
     * Sets the base address of the local BBjShip.
     *
     * @param deploymentUrl the base address
     *
     * @return this builder
     * @throws NullPointerException if deploymentUrl is null
     */
    public Builder setDeploymentUrl(URI deploymentUrl) {
      this.deploymentUrl = Objects.requireNonNull(deploymentUrl);
      return this;
    }

    /**
     * Sets the name the development run is published under.
     *
     * @param publishName the publish name
     *
     * @return this builder
     * @throws NullPointerException if publishName is null
     */
    public Builder setPublishName(String publishName) {
      this.publishName = Objects.requireNonNull(publishName);
      return this;
    }

    /**
     * Sets the administration credentials of the setup and the deployment.
     *
     * @param credentials the credentials
     *
     * @return this builder
     * @throws NullPointerException if credentials is null
     */
    public Builder setCredentials(BBjCredentials credentials) {
      this.credentials = Objects.requireNonNull(credentials);
      return this;
    }

    /**
     * Sets the local BBj installation directory.
     *
     * <p>
     * Without it the setup detects the installation.
     * </p>
     *
     * @param home the BBj home
     *
     * @return this builder
     * @throws NullPointerException if home is null
     */
    public Builder setHome(Path home) {
      this.home = Objects.requireNonNull(home);
      return this;
    }

    /**
     * Sets the hotswap settings BBjServices must run with.
     *
     * <p>
     * Without them the setup leaves the Java settings of BBjServices as they are.
     * </p>
     *
     * @param javaSettings the Java settings
     *
     * @return this builder
     * @throws NullPointerException if javaSettings is null
     */
    public Builder setJavaSettings(JavaSettings javaSettings) {
      this.javaSettings = Objects.requireNonNull(javaSettings);
      return this;
    }

    /**
     * Sets the deployment archive the development run is written to.
     *
     * @param archive the archive
     *
     * @return this builder
     * @throws NullPointerException if archive is null
     */
    public Builder setArchive(Path archive) {
      this.archive = Objects.requireNonNull(archive);
      return this;
    }

    /**
     * Sets the class folders the BBj machine loads the application from, in classpath order.
     *
     * @param classFolders the absolute class folders as the BBj machine sees them
     *
     * @return this builder
     * @throws NullPointerException if classFolders or one of its folders is null
     * @throws IllegalArgumentException if a folder is not absolute
     */
    public Builder setClassFolders(List<Path> classFolders) {
      for (Path folder : classFolders) {
        if (!Objects.requireNonNull(folder).isAbsolute()) {
          throw new IllegalArgumentException(
              "a class folder of a development run must be absolute: " + folder);
        }
      }

      this.classFolders = List.copyOf(classFolders);
      return this;
    }

    /**
     * Sets the resolved classpath of the application, the devtools jars it lacks are added to the
     * development run.
     *
     * @param applicationClasspath the application classpath
     *
     * @return this builder
     * @throws NullPointerException if applicationClasspath is null
     */
    public Builder setApplicationClasspath(ApplicationClasspath applicationClasspath) {
      this.applicationClasspath = Objects.requireNonNull(applicationClasspath);
      return this;
    }

    /**
     * Sets the runtime classpath of the application, the dependencies the development run carries.
     *
     * @param runtimeClasspath the runtime classpath
     *
     * @return this builder
     * @throws NullPointerException if runtimeClasspath is null
     */
    public Builder setRuntimeClasspath(ApplicationClasspath runtimeClasspath) {
      this.runtimeClasspath = Objects.requireNonNull(runtimeClasspath);
      return this;
    }

    /**
     * Sets the resolver the devtools are resolved through.
     *
     * @param resolver the resolver
     *
     * @return this builder
     * @throws NullPointerException if resolver is null
     */
    public Builder setResolver(ArtifactResolver resolver) {
      this.resolver = Objects.requireNonNull(resolver);
      return this;
    }

    /**
     * Sets where progress is reported.
     *
     * @param log the progress channel
     *
     * @return this builder
     * @throws NullPointerException if log is null
     */
    public Builder setLog(Consumer<String> log) {
      this.log = Objects.requireNonNull(log);
      return this;
    }

    /**
     * Sets where warnings are reported.
     *
     * @param warn the warning channel
     *
     * @return this builder
     * @throws NullPointerException if warn is null
     */
    public Builder setWarn(Consumer<String> warn) {
      this.warn = Objects.requireNonNull(warn);
      return this;
    }

    /**
     * Sets where details of the devtools resolution are reported.
     *
     * @param debug the detail channel
     *
     * @return this builder
     * @throws NullPointerException if debug is null
     */
    public Builder setDebug(Consumer<String> debug) {
      this.debug = Objects.requireNonNull(debug);
      return this;
    }

    /**
     * Builds the development run.
     *
     * @return the development run
     */
    public Development build() {
      return new Development(this);
    }
  }
}
