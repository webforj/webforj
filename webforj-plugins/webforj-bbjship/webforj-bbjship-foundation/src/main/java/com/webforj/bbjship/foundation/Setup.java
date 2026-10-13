package com.webforj.bbjship.foundation;

import com.webforj.bbjship.foundation.model.BBjCredentials;
import com.webforj.bbjship.foundation.model.JavaSettings;
import com.webforj.bbjship.foundation.model.VersionResponse;
import java.io.IOException;
import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.URI;
import java.net.UnknownHostException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.time.Duration;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;
import java.util.zip.ZipException;

/**
 * Installs and configures BBjShip in a local BBj installation.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class Setup {

  private static final String LAUNCHER_SCRIPT = "bbjship";
  private static final List<String> REQUIRED_FILES =
      List.of("bbj-plugin.json", "version.properties", LAUNCHER_SCRIPT, BBjShipPlugin.LIBRARY);
  private static final String HTTPS = "https";
  private static final Duration COMMAND_TIMEOUT = Duration.ofSeconds(120);
  private static final Duration RESTART_TIMEOUT = Duration.ofSeconds(450);
  private static final Duration POLL_INTERVAL = Duration.ofSeconds(2);
  private static final Duration PROGRESS_INTERVAL = Duration.ofSeconds(10);
  private static final String LAUNCHER =
      System.getProperty("os.name", "").startsWith("Windows") ? LAUNCHER_SCRIPT + ".cmd"
          : LAUNCHER_SCRIPT;

  private final Path bundle;
  private final String version;
  private final Path home;
  private final URI deploymentUrl;
  private final BBjCredentials credentials;
  private final int port;
  private final boolean force;
  private final boolean developmentEnabled;
  private final JavaSettings javaSettings;
  private final Duration commandTimeout;
  private final Duration restartTimeout;
  private final Duration pollInterval;
  private final Duration progressInterval;
  private final Consumer<String> log;
  private final Consumer<String> warn;

  private Setup(Builder builder) {
    this.bundle = builder.bundle;
    this.version = builder.version;
    this.home = builder.home;
    this.deploymentUrl = builder.deploymentUrl;
    this.credentials = builder.credentials;
    this.port = builder.port;
    this.force = builder.force;
    this.developmentEnabled = builder.developmentEnabled;
    this.javaSettings = builder.javaSettings;
    this.commandTimeout = builder.commandTimeout;
    this.restartTimeout = builder.restartTimeout;
    this.pollInterval = builder.pollInterval;
    this.progressInterval = builder.progressInterval;
    this.log = builder.log;
    this.warn = builder.warn;
  }

  /**
   * Creates a new builder for the setup of a complete BBjShip package.
   *
   * @param bundle the BBjShip ZIP
   * @param version the required webforJ version
   *
   * @return a new builder
   * @throws NullPointerException if an argument is null
   */
  public static Builder create(Path bundle, String version) {
    return new Builder(bundle, version);
  }

  /**
   * Installs BBjShip and configures local BBjServices.
   *
   * @throws IOException if setup is invalid or installation or configuration fails
   * @throws IllegalArgumentException if the deployment URL is no valid BBjShip address
   */
  public void apply() throws IOException {
    if (credentials.isToken()) {
      throw new IOException("BBj setup requires the administration password. "
          + "Configure bbj.admin.password instead of a token");
    }

    BBjShipClient bbjShip = BBjShipClient.create(deploymentUrl).setCredentials(credentials).build();
    applyLocalValidation();
    Path installation = Installation.resolveHome(home);
    BBjShipPlugin plugin = new BBjShipPlugin(installation);
    plugin.applyOwnershipValidation();
    String installed = readInstalledVersion(plugin.getDirectory());
    if (installed != null && !version.equals(installed) && !force) {
      throw new IOException("Installed BBjShip version " + installed + " does not match " + version
          + ". Run webforj:bbj-setup with -Dwebforj.bbj.force=true to replace it");
    }

    Path runtime = plugin.getRuntime();
    Files.createDirectories(runtime);
    boolean current = version.equals(installed) && !force;
    Path workspace = FileTree.createPrivateDirectory(runtime);
    try {
      Path incoming = workspace.resolve("package");
      if (!current) {
        DeploymentArchive.read(bundle, incoming, DeploymentArchive.MAX_BYTES,
            Setup::applyPluginEntryValidation);
        applyPackageValidation(incoming);
      }

      log.accept("Using BBj home " + installation);
      if (current) {
        log.accept("BBjShip " + version + " is already installed");
        applyConfiguration(installation, plugin, workspace, bbjShip);
      } else {
        applyInstallation(installation, plugin, incoming, workspace, installed, bbjShip);
      }

      if (javaSettings != null) {
        applyJavaSettings(installation, plugin, workspace, bbjShip);
      }

      log.accept("BBj setup completed");
    } finally {
      FileTree.removeDirectory(workspace);
    }
  }

  private void applyLocalValidation() throws IOException {
    applyEndpointValidation();
    if (port < 1 || port > 65535) {
      throw new IOException("BBj setup requires a web server port from 1 to 65535");
    }
  }

  private void applyEndpointValidation() throws IOException {
    InetAddress[] addresses;
    try {
      addresses = InetAddress.getAllByName(deploymentUrl.getHost());
    } catch (UnknownHostException unknown) {
      throw new IOException("BBj setup cannot resolve the deployment URL host "
          + deploymentUrl.getHost() + ". Use localhost or a name of this machine", unknown);
    }

    for (InetAddress address : addresses) {
      if (!address.isLoopbackAddress() && NetworkInterface.getByInetAddress(address) == null) {
        throw new IOException("BBj setup is local only. Run setup on the BBj host");
      }
    }
  }

  private String readInstalledVersion(Path destination) throws IOException {
    if (!Files.exists(destination)) {
      return null;
    }

    try {
      return BBjShipVersionFile.readVersion(destination);
    } catch (IOException failure) {
      if (!force) {
        throw new IOException(
            "Installed BBjShip version is missing. "
                + "Run webforj:bbj-setup with -Dwebforj.bbj.force=true to replace the package",
            failure);
      }

      return "unknown";
    }
  }

  private void applyPackageValidation(Path directory) throws IOException {
    for (String required : REQUIRED_FILES) {
      if (!Files.isRegularFile(directory.resolve(required))) {
        throw new IOException("BBjShip package is incomplete. Missing " + required);
      }
    }

    if (!version.equals(BBjShipVersionFile.readVersion(directory))) {
      throw new IOException("BBjShip package does not match webforJ " + version);
    }

    if (!directory.resolve(LAUNCHER_SCRIPT).toFile().setExecutable(true, true)) {
      throw new IOException("Cannot make the BBjShip command executable");
    }
  }

  private void applyInstallation(Path installation, BBjShipPlugin plugin, Path incoming,
      Path workspace, String installed, BBjShipClient bbjShip) throws IOException {
    Path destination = plugin.getDirectory();
    Path transaction = FileTree.createPrivateDirectory(plugin.getRuntime());
    Path replacement = transaction.resolve("replacement");
    Path backup = transaction.resolve("previous");
    InstallationStep step = InstallationStep.NONE;
    boolean configured = false;
    try {
      Files.createDirectory(replacement);
      FileTree.writeTree(incoming, replacement);
      log.accept(installed == null ? "Installing BBjShip " + version
          : "Replacing BBjShip " + installed + " with " + version);
      if (installed != null) {
        Files.move(destination, backup, StandardCopyOption.ATOMIC_MOVE);
        step = InstallationStep.PREVIOUS_MOVED;
      }

      Files.move(replacement, destination, StandardCopyOption.ATOMIC_MOVE);
      step = InstallationStep.REPLACEMENT_MOVED;
      applyConfiguration(installation, plugin, workspace, bbjShip);
      configured = true;
    } catch (IOException | RuntimeException failure) {
      applyRollback(failure, destination, backup, step, installed != null);
      throw failure;
    } finally {
      if (!configured && Files.exists(backup)) {
        warn.accept("Previous BBjShip files are preserved at " + backup);
      } else {
        FileTree.removeDirectory(transaction);
      }
    }
  }

  private void applyRollback(Exception failure, Path destination, Path backup,
      InstallationStep step, boolean previous) {
    if (failure instanceof CommandTerminationException) {
      warn.accept("BBj setup command is still running. Plugin files have been preserved");
      return;
    }

    try {
      applyRestore(destination, backup, step, previous);
    } catch (IOException cleanup) {
      failure.addSuppressed(cleanup);
    }
  }

  private void applyRestore(Path destination, Path backup, InstallationStep step, boolean previous)
      throws IOException {
    if (step == InstallationStep.REPLACEMENT_MOVED) {
      FileTree.removeDirectory(destination);
    }

    if (previous && step != InstallationStep.NONE) {
      Files.move(backup, destination, StandardCopyOption.ATOMIC_MOVE);
    }
  }

  private void applyConfiguration(Path installation, BBjShipPlugin plugin, Path workspace,
      BBjShipClient bbjShip) throws IOException {
    String launcher = plugin.getDirectory().resolve(LAUNCHER).toString();
    // A forced setup replaced the BBjShip jar, which BBjServices loads again only through the
    // classpath reset of remote enable, so the answer of the running endpoint does not count
    boolean enabled = !force && findVersion(bbjShip).map(VersionResponse::getVersion)
        .filter(version::equals).isPresent();
    if (enabled) {
      log.accept("BBjShip endpoint is enabled");
    } else {
      String portOption =
          HTTPS.equalsIgnoreCase(deploymentUrl.getScheme()) ? BBjShipCommandLine.SSL_PORT_OPTION
              : BBjShipCommandLine.PORT_OPTION;
      log.accept("Enabling BBj remote deployment");
      startCommand(installation, workspace, List.of(launcher, BBjShipCommandLine.REMOTE,
          BBjShipCommandLine.ENABLE, portOption, Integer.toString(port)), commandTimeout);
    }

    if (developmentEnabled && !Files.exists(plugin.getDevelopmentSwitch())) {
      log.accept("Enabling BBj development runs");
      startCommand(installation, workspace,
          List.of(launcher, BBjShipCommandLine.DEVELOPMENT, BBjShipCommandLine.ENABLE),
          commandTimeout);
    }
  }

  private void applyJavaSettings(Path installation, BBjShipPlugin plugin, Path workspace,
      BBjShipClient bbjShip) throws IOException {
    Path file = javaSettings.getPropertiesFile()
        .orElseGet(() -> ServicesProperties.resolveDefaultFile(installation));
    Path optionsDirectory = javaSettings.getOptionsDirectory();
    String argument = ContainerOptionsFile.getArgument(optionsDirectory);
    ServicesProperties properties = ServicesProperties.load(file, plugin.getRuntime());
    if (!properties.hasJavaSettings(argument)) {
      log.accept("Updating the Java settings of BBjServices in " + file);
      properties.applyJavaSettings(argument);
    }

    List<String> lines = ContainerOptionsFile.readLines(optionsDirectory);
    Optional<VersionResponse> running = findVersion(bbjShip);
    if (running.isPresent() && isHotswapRuntime(running.get(), lines)) {
      log.accept("BBjServices runs with the hotswap settings");
      return;
    }

    log.accept("Restarting BBjServices");
    ServicesProcess services = new ServicesProcess(plugin);
    services.setLog(log);
    services.applyStartValidation();
    PollingWait restartWait = PollingWait.create("BBj setup", restartTimeout, pollInterval)
        .setLog(log).setProgressMessage("Waiting for BBjServices, ")
        .setProgressInterval(progressInterval).build();
    startCommand(installation, workspace,
        List.of(plugin.getDirectory().resolve(LAUNCHER).toString(), BBjShipCommandLine.SERVICES,
            BBjShipCommandLine.STOP),
        process -> restartWait.apply(() -> !process.isAlive()).isPresent());
    services.applyRestart(bbjShip);
    if (!isHotswapRuntime(bbjShip.getVersion(), lines)) {
      throw new IOException("BBjServices restarted without the hotswap settings. Check " + file);
    }
  }

  private Optional<VersionResponse> findVersion(BBjShipClient bbjShip) {
    try {
      return Optional.of(bbjShip.getVersion());
    } catch (IOException unanswered) {
      return Optional.empty();
    }
  }

  private void startCommand(Path installation, Path workspace, List<String> command,
      Duration timeout) throws IOException {
    startCommand(installation, workspace, command,
        process -> process.waitFor(timeout.toSeconds(), TimeUnit.SECONDS));
  }

  private void startCommand(Path installation, Path workspace, List<String> command,
      CommandWait wait) throws IOException {
    Path output = FileTree.createPrivateFile(workspace, "command", ".log");
    Process process = null;
    try {
      ProcessBuilder builder = new ProcessBuilder(command).directory(installation.toFile())
          .redirectErrorStream(true).redirectOutput(output.toFile());
      builder.environment().put("NO_COLOR", "1");
      builder.environment().put(BBjShipCommandLine.USERNAME_VARIABLE, credentials.getUsername());
      builder.environment().put(BBjShipCommandLine.PASSWORD_VARIABLE, credentials.getPassword());
      process = builder.start();
      process.getOutputStream().close();

      if (!wait.isFinished(process)) {
        throw new IOException("BBj setup command timed out. Check local BBjServices");
      }

      if (process.exitValue() != 0) {
        String detail = String.join(" ", Files.readAllLines(output)).strip();
        throw new IOException("BBj setup command failed. Check credentials and local BBjServices."
            + (detail.isEmpty() ? "" : " " + detail));
      }
    } catch (InterruptedException failure) {
      Thread.currentThread().interrupt();
      throw closeCommand(process, output, new IOException("BBj setup was interrupted", failure));
    } catch (IOException failure) {
      throw closeCommand(process, output, failure);
    }

    Files.deleteIfExists(output);
  }

  private IOException closeCommand(Process process, Path output, IOException failure) {
    IOException result = failure;
    if (process != null && process.isAlive()) {
      try {
        stopCommand(process);
      } catch (CommandTerminationException termination) {
        termination.addSuppressed(result);
        result = termination;
      }
    }

    try {
      Files.deleteIfExists(output);
    } catch (IOException cleanup) {
      result.addSuppressed(cleanup);
    }

    return result;
  }

  private void stopCommand(Process process) throws CommandTerminationException {
    boolean interrupted = Thread.interrupted();
    try {
      process.destroyForcibly();
      if (!process.waitFor(10, TimeUnit.SECONDS)) {
        throw new CommandTerminationException();
      }
    } catch (InterruptedException failure) {
      interrupted = true;
      throw new CommandTerminationException();
    } finally {
      if (interrupted) {
        Thread.currentThread().interrupt();
      }
    }
  }

  private static void applyPluginEntryValidation(String name) throws IOException {
    if (DeploymentArchive.isUnsafePath(name)) {
      throw new ZipException("Invalid BBj plugin archive entry: " + name);
    }

    if (BBjShipPlugin.RUNTIME_NAMES.contains(name.split("/")[0])) {
      throw new ZipException("BBj plugin archive must not contain runtime files");
    }
  }

  private static boolean isHotswapRuntime(VersionResponse answer, List<String> lines) {
    return answer.getJavaArguments().containsAll(lines);
  }

  /**
   * Builds a {@link Setup}.
   *
   * @author Hyyan Abo Fakher
   * @since 26.03
   */
  public static final class Builder {

    private final Path bundle;
    private final String version;
    private Path home;
    private URI deploymentUrl;
    private BBjCredentials credentials;
    private int port;
    private boolean force;
    private boolean developmentEnabled;
    private JavaSettings javaSettings;
    private Duration commandTimeout = COMMAND_TIMEOUT;
    private Duration restartTimeout = RESTART_TIMEOUT;
    private Duration pollInterval = POLL_INTERVAL;
    private Duration progressInterval = PROGRESS_INTERVAL;
    private Consumer<String> log = line -> {
    };
    private Consumer<String> warn = line -> {
    };

    private Builder(Path bundle, String version) {
      this.bundle = Objects.requireNonNull(bundle).toAbsolutePath().normalize();
      this.version = Objects.requireNonNull(version);
    }

    /**
     * Sets the local BBj installation directory.
     *
     * <p>
     * Without it the installation is taken from the BBJ_HOME variable, the running BBjServices, the
     * install file of the installer, a bbj executable on the search path or the default folder of
     * the installer, in that order.
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
     * Sets the BBj administration credentials.
     *
     * @param credentials the administration credentials
     *
     * @return this builder
     * @throws NullPointerException if credentials is null
     */
    public Builder setCredentials(BBjCredentials credentials) {
      this.credentials = Objects.requireNonNull(credentials);
      return this;
    }

    /**
     * Sets the port of the BBjServices web server the deployment URL addresses.
     *
     * <p>
     * The port is the HTTP port for an {@code http} URL and the SSL port for an {@code https} URL.
     * </p>
     *
     * @param port the web server port
     *
     * @return this builder
     */
    public Builder setPort(int port) {
      this.port = port;
      return this;
    }

    /**
     * Sets whether an existing BBjShip may be replaced.
     *
     * @param force whether to replace the installed package
     *
     * @return this builder
     */
    public Builder setForce(boolean force) {
      this.force = force;
      return this;
    }

    /**
     * Sets whether development runs are enabled on BBjShip.
     *
     * @param developmentEnabled whether to enable development runs
     *
     * @return this builder
     */
    public Builder setDevelopmentEnabled(boolean developmentEnabled) {
      this.developmentEnabled = developmentEnabled;
      return this;
    }

    /**
     * Sets the hotswap options BBjServices must run with.
     *
     * <p>
     * Without them the properties file of BBjServices is left alone and BBjServices is not
     * restarted.
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
     * Sets how long the remote enable and development enable commands may run.
     *
     * @param commandTimeout the command timeout, 120 seconds by default
     *
     * @return this builder
     * @throws NullPointerException if commandTimeout is null
     */
    public Builder setCommandTimeout(Duration commandTimeout) {
      this.commandTimeout = Objects.requireNonNull(commandTimeout);
      return this;
    }

    /**
     * Sets how long the restart command of BBjServices may run.
     *
     * @param restartTimeout the restart timeout, 450 seconds by default
     *
     * @return this builder
     * @throws NullPointerException if restartTimeout is null
     */
    public Builder setRestartTimeout(Duration restartTimeout) {
      this.restartTimeout = Objects.requireNonNull(restartTimeout);
      return this;
    }

    /**
     * Sets how long to wait between two checks of the restart command of BBjServices.
     *
     * @param pollInterval the poll interval, 2 seconds by default
     *
     * @return this builder
     * @throws NullPointerException if pollInterval is null
     */
    public Builder setPollInterval(Duration pollInterval) {
      this.pollInterval = Objects.requireNonNull(pollInterval);
      return this;
    }

    /**
     * Sets how often the wait for BBjServices is reported during a restart of BBjServices.
     *
     * @param progressInterval the progress interval, 10 seconds by default
     *
     * @return this builder
     * @throws NullPointerException if progressInterval is null
     */
    public Builder setProgressInterval(Duration progressInterval) {
      this.progressInterval = Objects.requireNonNull(progressInterval);
      return this;
    }

    /**
     * Sets where setup actions are reported.
     *
     * @param log the log sink
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
     * @param warn the warning sink
     *
     * @return this builder
     * @throws NullPointerException if warn is null
     */
    public Builder setWarn(Consumer<String> warn) {
      this.warn = Objects.requireNonNull(warn);
      return this;
    }

    /**
     * Builds the setup.
     *
     * @return the setup
     */
    public Setup build() {
      return new Setup(this);
    }
  }

  /**
   * Reports a setup command that has not stopped.
   *
   * @author Hyyan Abo Fakher
   * @since 26.03
   */
  private static final class CommandTerminationException extends IOException {

    private static final long serialVersionUID = 1L;

    private CommandTerminationException() {
      super("BBj setup command did not stop. Resolve the running setup command before retrying");
    }
  }

  /**
   * Waits for a started setup command.
   *
   * @author Hyyan Abo Fakher
   * @since 26.03
   */
  @FunctionalInterface
  private interface CommandWait {

    boolean isFinished(Process process) throws IOException, InterruptedException;
  }

  /**
   * Names the last completed step of a BBjShip installation.
   *
   * @author Hyyan Abo Fakher
   * @since 26.03
   */
  private enum InstallationStep {
    NONE, PREVIOUS_MOVED, REPLACEMENT_MOVED
  }
}
