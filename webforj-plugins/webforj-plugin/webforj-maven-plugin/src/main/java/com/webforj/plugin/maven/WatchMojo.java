package com.webforj.plugin.maven;

import com.webforj.bundle.bun.BundleLogger;
import com.webforj.bundle.bun.BundlerExecution;
import com.webforj.bundle.bun.WatchSession;
import com.webforj.plugin.foundation.WatchConfigGuard;
import com.webforj.plugin.foundation.WatchPortFile;
import com.webforj.plugin.foundation.WatchProtocol;
import com.webforj.plugin.foundation.WatchSocketServer;
import com.webforj.plugin.foundation.bbj.Development;
import com.webforj.plugin.foundation.bbj.ServicesProperties;
import com.webforj.plugin.foundation.bbj.model.BBjCredentials;
import com.webforj.plugin.foundation.bbj.model.JavaSettings;
import com.webforj.plugin.foundation.hotswap.HotswapLaunch;
import com.webforj.plugin.foundation.resolve.ArtifactResolver;
import com.webforj.plugin.maven.bbj.DeploymentSettings;
import com.webforj.plugin.maven.bbj.ProjectDeployment;
import com.webforj.plugin.maven.bbj.model.BBjOptions;
import com.webforj.plugin.maven.devtools.SpringDevtoolsInjection;
import com.webforj.plugin.maven.hotswap.HotswapInjection;
import com.webforj.plugin.maven.hotswap.HotswapOptions;
import com.webforj.plugin.maven.resolve.MavenArtifacts;
import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;
import java.util.Properties;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.atomic.AtomicReference;
import javax.inject.Inject;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.descriptor.PluginDescriptor;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.toolchain.Toolchain;
import org.apache.maven.toolchain.ToolchainManager;
import org.eclipse.aether.RepositorySystem;
import org.eclipse.aether.RepositorySystemSession;
import org.eclipse.aether.resolution.ArtifactResolutionException;

/**
 * Goal that runs the development bundle watch in the stable Maven process and forwards its output
 * to the running application over a socket.
 *
 * <p>
 * The goal builds the frontend once, starts the Bun watcher, and returns so the application run
 * goal that follows it on the same command line can start. The watcher and the socket live on
 * background threads for the lifetime of the Maven process, so they are reused across every
 * application restart without ever being rebuilt. A shutdown hook stops the watcher and removes the
 * discovery file when the Maven process exits.
 * </p>
 *
 * <p>
 * The goal grabs a free port, writes it to a discovery file keyed by the project path, and listens
 * on it. The application reads that file on start, connects, and reconnects after every restart.
 * The watcher's log lines and its rebuild events both travel over that socket, so the bundler
 * output appears in the application log and a rebuild reaches the browser reload, identically under
 * every runtime.
 * </p>
 *
 * <p>
 * With a BBj deployment URL configured, the goal sets up BBjShip on the local BBj installation for
 * development runs, starts a development run of the application on BBjShip and stops it when the
 * Maven process exits.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
@Mojo(name = "watch", requiresDependencyResolution = ResolutionScope.COMPILE_PLUS_RUNTIME,
    threadSafe = true)
public class WatchMojo extends AbstractBundlerMojo {

  /**
   * The hotswap integration, naming the tool to attach.
   *
   * <p>
   * When a tool is named, this goal places its agent arguments into the properties the application
   * run goal reads for its fork, so the agent enters only the application virtual machine.
   * </p>
   */
  @Parameter
  protected HotswapOptions hotswap;

  /**
   * Command line selection of the hotswap tool, {@code jrebel} or {@code off}.
   *
   * <p>
   * When given, this wins over the project configuration for the run.
   * </p>
   */
  @Parameter(property = HotswapLaunch.SELECTION_PROPERTY)
  protected String hotswapSelection;

  /**
   * Command line hotswap directory.
   *
   * <p>
   * When given, this wins over the directory of the hotswap configuration for the run.
   * </p>
   */
  @Parameter(property = HotswapLaunch.DIRECTORY_PROPERTY)
  protected File hotswapDirectory;

  /** The current Maven session, the source of the command line properties. */
  @Parameter(defaultValue = "${session}", readonly = true, required = true)
  protected MavenSession session;

  /** The execution of this goal, matched against the goals of the build. */
  @Parameter(defaultValue = "${mojoExecution}", readonly = true, required = true)
  protected MojoExecution mojoExecution;

  /** The descriptor of this plugin, the source of the webforJ version BBjShip must match. */
  @Parameter(defaultValue = "${plugin}", readonly = true, required = true)
  protected PluginDescriptor pluginDescriptor;

  @Parameter
  BBjOptions bbj = new BBjOptions();

  @Parameter(property = "webforj.bbj.deployment.url")
  String deploymentUrl;

  @Parameter(property = "webforj.bbj.admin.username")
  String username;

  @Parameter(property = "webforj.bbj.admin.password")
  String password;

  @Parameter(property = "webforj.bbj.admin.token")
  String token;

  @Parameter(property = "webforj.bbj.publishName")
  String publishName;

  @Parameter(property = "webforj.bbj.home")
  File home;

  @Parameter(property = "webforj.bbj.properties")
  File properties;

  /** The repository session of the build. */
  @Parameter(defaultValue = "${repositorySystemSession}", readonly = true, required = true)
  protected RepositorySystemSession repositorySession;

  private final ToolchainManager toolchainManager;
  private final RepositorySystem repositorySystem;
  private List<String> hotswapArguments = List.of();
  private boolean sharedContainer;
  private Path containerOptionsDirectory;

  /**
   * Creates the goal with the build components it works with.
   *
   * @param toolchainManager the toolchain manager that names the virtual machine the application
   *        run goal forks
   * @param repositorySystem the repository system the devtools dependency tree and the BBjShip
   *        package are resolved through
   */
  @Inject
  public WatchMojo(ToolchainManager toolchainManager, RepositorySystem repositorySystem) {
    this.toolchainManager = toolchainManager;
    this.repositorySystem = repositorySystem;
  }

  @Override
  public void execute() throws MojoExecutionException {
    // The agent attachment and the devtools delivery do not depend on the frontend, so they happen
    // before the watch decides whether there is anything to bundle.
    attachHotswap();
    deliverDevtools();
    Optional<DevelopmentRun> run = startDevelopmentRun();
    AtomicReference<Runnable> watchStop = new AtomicReference<>(() -> {
    });
    installShutdownHook(run, watchStop);

    if (!sourceRoot.isDirectory()) {
      getLog().info("no bundle source root at " + sourceRoot + ", skipping the watch");
      if (run.isPresent()) {
        handleLastGoal(run);
      }

      return;
    }

    Path portFile = WatchPortFile.resolve(project.getBasedir().getAbsolutePath());
    WatchSocketServer socket = openSocket(portFile);

    // Forward this through the socket rather than the build console, so it reaches the application
    // log with the rest of the watch output once the application connects.
    socket.send(WatchProtocol.log("webforj watch listening on port " + socket.getPort()));

    // The application reads the generated webforJ configuration from the build output directory on
    // every restart, and an IDE build can remove it there between restarts. The guard lives in this
    // process, which survives every restart, and writes the file back the moment it disappears.
    WatchConfigGuard configGuard = startConfigGuard(socket);

    // The initial blocking build reports to the build console, since no application is connected
    // yet and a failure there must land where the developer is looking. Once the watcher is up the
    // sink flips to the socket, so every later line reaches the running application log.
    AtomicReference<BundleLogger> sink = new AtomicReference<>(new MavenBundleLogger(getLog()));

    BundlerExecution execution = createExecution();
    try {
      WatchSession watchSession =
          execution.watch(createRequest(), changed -> socket.send(WatchProtocol.rebuild(changed)),
              (level, line) -> sink.get().log(level, line));
      sink.set((level, line) -> socket
          .send(level == System.Logger.Level.WARNING ? WatchProtocol.warn(line)
              : WatchProtocol.log(line)));
      // The application rescans for new bundle entries every time it connects, which is every
      // development restart.
      if (watchSession != null) {
        socket.setOnConnect(watchSession::rescan);
      }

      watchStop.set(() -> {
        if (watchSession != null) {
          watchSession.close();
        }

        closeGuard(configGuard);
        closeSocket(socket, portFile);
      });
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();
      closeGuard(configGuard);
      closeSocket(socket, portFile);
      throw new MojoExecutionException("the initial watch build was interrupted", e);
    } catch (Exception e) {
      closeGuard(configGuard);
      closeSocket(socket, portFile);
      throw new MojoExecutionException("the watch failed to start: " + e.getMessage(), e);
    }

    handleLastGoal(run);
  }

  Optional<DevelopmentRun> startDevelopmentRun() throws MojoExecutionException {
    DeploymentSettings settings = createDeploymentSettings();
    if (settings.getUrl().isBlank()) {
      return Optional.empty();
    }

    settings.setDefaultPublishName(project.getBuild().getFinalName())
        .setVersion(pluginDescriptor.getVersion());
    String name = settings.getPublishName();
    BBjCredentials credentials = settings.getCredentials();
    if (credentials.isFactoryAccount()) {
      getLog().warn(BBjCredentials.FACTORY_ACCOUNT_WARNING);
    }

    Path archive = ProjectDeployment.resolveArchive(project, name);
    try {
      Path bundle = MavenArtifacts.resolveBBjShipPackage(repositorySystem, repositorySession,
          project.getRemoteProjectRepositories(), settings.getVersion());
      File configured = bbj.getDevelopment().getClassFolder();
      Path classFolder = (configured != null ? configured.toPath()
          : Path.of(project.getBuild().getOutputDirectory())).toAbsolutePath().normalize();
      Development.Builder development = Development.create(bundle, settings.getVersion())
          .setDeploymentUrl(URI.create(settings.getUrl())).setPublishName(name)
          .setCredentials(credentials).setArchive(archive).setClassFolders(List.of(classFolder))
          .setApplicationClasspath(MavenArtifacts.getApplicationClasspath(project))
          .setRuntimeClasspath(MavenArtifacts.getRuntimeClasspath(project))
          .setResolver(getArtifactResolver()).setLog(getLog()::info).setWarn(getLog()::warn)
          .setDebug(getLog()::debug);
      File effectiveHome = settings.getHome();
      if (effectiveHome != null) {
        development.setHome(effectiveHome.toPath());
      }

      if (sharedContainer && !hotswapArguments.isEmpty()) {
        File effectiveProperties = settings.getProperties();
        development.setJavaSettings(
            new JavaSettings(effectiveProperties == null ? null : effectiveProperties.toPath(),
                containerOptionsDirectory));
      }

      Development built = development.build();
      URI address = built.start();

      return Optional.of(new DevelopmentRun(built, name, address));
    } catch (ArtifactResolutionException | IOException | IllegalArgumentException failure) {
      throw new MojoExecutionException(Development.FAILURE, failure);
    }
  }

  void stopDevelopmentRun(DevelopmentRun run) {
    run.development.stop();
  }

  private DeploymentSettings createDeploymentSettings() {
    return DeploymentSettings.create(bbj).setUrl(deploymentUrl).setPublishName(publishName)
        .setUsername(username).setPassword(password).setToken(token).setHome(home)
        .setProperties(properties);
  }

  private void handleLastGoal(Optional<DevelopmentRun> run) {
    run.ifPresent(started -> getLog().info(
        "webforJ watch: " + started.getPublishName() + " is running at " + started.getAddress()));
    if (!isLastGoal()) {
      return;
    }

    getLog().info(run.isPresent() ? "webforJ watch: press Ctrl+C to stop the app and the watch"
        : "the watch is the last goal of the build and keeps it running, "
            + "stop the build to stop the watch");
    try {
      new CountDownLatch(1).await();
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();
    }
  }

  private boolean isLastGoal() {
    if (session == null || mojoExecution == null) {
      return false;
    }

    List<String> goals = session.getGoals();
    if (goals == null || goals.isEmpty()) {
      String defaultGoal = session.getTopLevelProject().getDefaultGoal();
      goals = defaultGoal == null || defaultGoal.isBlank() ? List.of()
          : List.of(defaultGoal.trim().split("\\s+"));
    }

    if (goals.isEmpty()) {
      return false;
    }

    String last = goals.get(goals.size() - 1);
    int executionId = last.indexOf('@');
    String[] parts = (executionId < 0 ? last : last.substring(0, executionId)).split(":");
    if (parts.length < 2 || !mojoExecution.getGoal().equals(parts[parts.length - 1])) {
      return false;
    }

    PluginDescriptor plugin = mojoExecution.getMojoDescriptor().getPluginDescriptor();
    if (parts.length == 2) {
      return plugin.getGoalPrefix().equals(parts[0]);
    }

    return plugin.getGroupId().equals(parts[0]) && plugin.getArtifactId().equals(parts[1]);
  }

  private void attachHotswap() throws MojoExecutionException {
    Properties userProperties = session == null ? new Properties() : session.getUserProperties();
    HotswapInjection.Builder builder = HotswapInjection.create().setProject(project)
        .setUserProperties(userProperties).setOptions(hotswap).setCommandLineValue(hotswapSelection)
        .setDirectory(hotswapDirectory == null ? null : hotswapDirectory.toPath())
        .setResolver(getArtifactResolver()).setBbjDeploymentUrl(createDeploymentSettings().getUrl())
        .setLog(getLog());
    sharedContainer = builder.isSharedContainer();
    boolean selected = hotswap != null || hotswapSelection != null && !hotswapSelection.isBlank();
    HotswapInjection injection = builder.setJavaExecutable(
        sharedContainer && selected ? resolveServicesJavaExecutable() : toolchainJavaExecutable())
        .build();
    hotswapArguments = injection.apply();
    containerOptionsDirectory = injection.getContainerOptionsDirectory();
  }

  private void deliverDevtools() throws MojoExecutionException {
    Properties userProperties = session == null ? new Properties() : session.getUserProperties();
    SpringDevtoolsInjection.create().setProject(project).setUserProperties(userProperties)
        .setResolver(getArtifactResolver()).setLog(getLog()).build().apply();
  }

  private ArtifactResolver getArtifactResolver() {
    return MavenArtifacts.getResolver(repositorySystem, repositorySession,
        project.getRemoteProjectRepositories());
  }

  private Path toolchainJavaExecutable() {
    if (toolchainManager == null || session == null) {
      return null;
    }

    // The application run goal forks the toolchain from the build context when one is configured,
    // otherwise this very virtual machine, so the capability check follows the same selection.
    Toolchain toolchain = toolchainManager.getToolchainFromBuildContext("jdk", session);
    if (toolchain == null) {
      return null;
    }

    String java = toolchain.findTool("java");
    return java == null || java.isBlank() ? null : Path.of(java);
  }

  private Path resolveServicesJavaExecutable() throws MojoExecutionException {
    DeploymentSettings settings = createDeploymentSettings();
    File configured = settings.getProperties();
    File effectiveHome = settings.getHome();
    try {
      return ServicesProperties
          .findJavaExecutable(effectiveHome == null ? null : effectiveHome.toPath(),
              configured == null ? null : configured.toPath())
          .orElse(null);
    } catch (IOException failure) {
      throw new MojoExecutionException(failure.getMessage(), failure);
    }
  }

  private WatchConfigGuard startConfigGuard(WatchSocketServer socket) {
    Path configFile = Path.of(project.getBuild().getOutputDirectory()).resolve("webforj.conf");
    try {
      return WatchConfigGuard.start(configFile, line -> socket.send(WatchProtocol.log(line)));
    } catch (IOException e) {
      getLog().warn("could not guard " + configFile + ": " + e.getMessage());

      return null;
    }
  }

  private static void closeGuard(WatchConfigGuard configGuard) {
    if (configGuard != null) {
      configGuard.close();
    }
  }

  private WatchSocketServer openSocket(Path portFile) throws MojoExecutionException {
    WatchSocketServer socket = null;
    try {
      socket = new WatchSocketServer();
      socket.start();
      Files.writeString(portFile, Integer.toString(socket.getPort()), StandardCharsets.UTF_8);

      return socket;
    } catch (IOException e) {
      if (socket != null) {
        closeSocket(socket, portFile);
      }

      throw new MojoExecutionException("could not open the watch socket: " + e.getMessage(), e);
    }
  }

  private void closeSocket(WatchSocketServer socket, Path portFile) {
    socket.close();
    try {
      // Another watch on the same project may have started since and rewritten the discovery
      // file with its own port. Removing it then would break the discovery for the live watch,
      // so the file only goes when it still names this watch.
      if (Files.isRegularFile(portFile) && String.valueOf(socket.getPort())
          .equals(Files.readString(portFile, StandardCharsets.UTF_8).trim())) {
        Files.delete(portFile);
      }
    } catch (IOException e) {
      getLog().debug("could not remove the watch discovery file " + portFile);
    }
  }

  private void installShutdownHook(Optional<DevelopmentRun> run,
      AtomicReference<Runnable> watchStop) {
    Runtime.getRuntime().addShutdownHook(new Thread(() -> {
      run.ifPresent(this::stopDevelopmentRun);
      watchStop.get().run();
    }, "webforj-watch-shutdown"));
  }

  /**
   * A development run of the application on BBjShip.
   *
   * @author Hyyan Abo Fakher
   * @since 26.03
   */
  static final class DevelopmentRun {

    private final Development development;
    private final String publishName;
    private final URI address;

    DevelopmentRun(Development development, String publishName, URI address) {
      this.development = development;
      this.publishName = publishName;
      this.address = address;
    }

    String getPublishName() {
      return publishName;
    }

    URI getAddress() {
      return address;
    }
  }
}
