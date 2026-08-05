package com.webforj.plugin.maven;

import com.webforj.bundle.bun.BundleLogger;
import com.webforj.bundle.bun.BundlerExecution;
import com.webforj.bundle.bun.WatchSession;
import com.webforj.plugin.foundation.WatchConfigGuard;
import com.webforj.plugin.foundation.WatchPortFile;
import com.webforj.plugin.foundation.WatchProtocol;
import com.webforj.plugin.foundation.WatchSocketServer;
import com.webforj.plugin.maven.devtools.SpringDevtoolsInjection;
import com.webforj.plugin.maven.hotswap.HotswapInjection;
import com.webforj.plugin.maven.hotswap.HotswapOptions;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicReference;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Component;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.toolchain.Toolchain;
import org.apache.maven.toolchain.ToolchainManager;
import org.eclipse.aether.RepositorySystem;
import org.eclipse.aether.RepositorySystemSession;

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
  @Parameter(property = HotswapInjection.SELECTION_PROPERTY)
  protected String hotswapSelection;

  /** The current Maven session, the source of the command line properties. */
  @Parameter(defaultValue = "${session}", readonly = true, required = true)
  protected MavenSession session;

  /** The toolchain manager that names the virtual machine the application run goal forks. */
  @Component
  protected ToolchainManager toolchainManager;

  /** The repository system the devtools dependency tree is resolved through. */
  @Component
  protected RepositorySystem repositorySystem;

  /** The repository session of the build. */
  @Parameter(defaultValue = "${repositorySystemSession}", readonly = true, required = true)
  protected RepositorySystemSession repositorySession;

  @Override
  public void execute() throws MojoExecutionException {
    // The agent attachment and the devtools delivery do not depend on the frontend, so they happen
    // before the watch decides whether there is anything to bundle.
    attachHotswap();
    deliverDevtools();

    if (!sourceRoot.isDirectory()) {
      getLog().info("no bundle source root at " + sourceRoot + ", skipping the watch");

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
      WatchSession session =
          execution.watch(createRequest(), changed -> socket.send(WatchProtocol.rebuild(changed)),
              (level, line) -> sink.get().log(level, line));
      sink.set((level, line) -> socket
          .send(level == System.Logger.Level.WARNING ? WatchProtocol.warn(line)
              : WatchProtocol.log(line)));
      // The application rescans for new bundle entries every time it connects, which is every
      // development restart.
      if (session != null) {
        socket.setOnConnect(session::rescan);
      }

      installShutdownHook(session, socket, portFile, configGuard);
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
  }

  private void attachHotswap() throws MojoExecutionException {
    Properties userProperties = session == null ? new Properties() : session.getUserProperties();
    HotswapInjection.create().setProject(project).setUserProperties(userProperties)
        .setOptions(hotswap).setCommandLineValue(hotswapSelection)
        .setJavaExecutable(toolchainJavaExecutable()).setLog(getLog()).build().apply();
  }

  private void deliverDevtools() throws MojoExecutionException {
    Properties userProperties = session == null ? new Properties() : session.getUserProperties();
    SpringDevtoolsInjection.create().setProject(project).setUserProperties(userProperties)
        .setResolver(SpringDevtoolsInjection.resolver(repositorySystem, repositorySession,
            project.getRemoteProjectRepositories()))
        .setLog(getLog()).build().apply();
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

  private void installShutdownHook(WatchSession session, WatchSocketServer socket, Path portFile,
      WatchConfigGuard configGuard) {
    Runtime.getRuntime().addShutdownHook(new Thread(() -> {
      if (session != null) {
        session.close();
      }

      closeGuard(configGuard);
      closeSocket(socket, portFile);
    }, "webforj-watch-shutdown"));
  }
}
