package com.webforj.plugin.maven;

import com.webforj.bundle.bun.BundlerExecution;
import com.webforj.plugin.foundation.WatchPortFile;
import com.webforj.plugin.foundation.WatchProtocol;
import com.webforj.plugin.foundation.WatchSocketServer;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.slf4j.event.Level;

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

  @Override
  public void execute() throws MojoExecutionException {
    if (!sourceRoot.isDirectory()) {
      getLog().info("no bundle source root at " + sourceRoot + ", skipping the watch");

      return;
    }

    Path portFile = WatchPortFile.resolve(project.getBasedir().getAbsolutePath());
    WatchSocketServer socket = openSocket(portFile);

    // Forward this through the socket rather than the build console, so it reaches the application
    // log with the rest of the watch output once the application connects.
    socket.send(WatchProtocol.log("webforj watch listening on port " + socket.getPort()));

    BundlerExecution execution = createExecution();
    try {
      Process watcher = execution.watch(createRequest(),
          changed -> socket.send(WatchProtocol.rebuild(changed)), (level, line) -> socket
              .send(level == Level.WARN ? WatchProtocol.warn(line) : WatchProtocol.log(line)));
      installShutdownHook(watcher, socket, portFile);
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();
      closeSocket(socket, portFile);
      throw new MojoExecutionException("the initial watch build was interrupted", e);
    } catch (Exception e) {
      closeSocket(socket, portFile);
      throw new MojoExecutionException("the watch failed to start: " + e.getMessage(), e);
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
      Files.deleteIfExists(portFile);
    } catch (IOException e) {
      getLog().debug("could not remove the watch discovery file " + portFile);
    }
  }

  private void installShutdownHook(Process watcher, WatchSocketServer socket, Path portFile) {
    Runtime.getRuntime().addShutdownHook(new Thread(() -> {
      if (watcher != null) {
        watcher.destroy();
      }

      closeSocket(socket, portFile);
    }, "webforj-watch-shutdown"));
  }
}
