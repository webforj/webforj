package com.webforj.plugin.gradle;

import com.webforj.plugin.foundation.WatchSocketServer;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.gradle.api.services.BuildService;
import org.gradle.api.services.BuildServiceParameters;

/**
 * Tears down the development watcher when the build that started it ends.
 *
 * <p>
 * The watch task starts a Bun watcher and a socket on background threads and returns, so they stay
 * alive for the application run that follows it. A JVM shutdown hook alone does not release them
 * under the Gradle daemon, which outlives the build, so the watcher would keep running and holding
 * its port. Gradle closes this service when the build ends, in the daemon as well, which stops the
 * watcher there.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public abstract class WatchLifecycle
    implements BuildService<BuildServiceParameters.None>, AutoCloseable {

  private Process watcher;
  private WatchSocketServer socket;
  private Path portFile;

  /**
   * Records the watcher resources to release when the build ends.
   *
   * @param watcher the running Bun watcher process
   * @param socket the watch socket server
   * @param portFile the discovery file keyed by the project path
   */
  synchronized void track(Process watcher, WatchSocketServer socket, Path portFile) {
    this.watcher = watcher;
    this.socket = socket;
    this.portFile = portFile;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public synchronized void close() {
    if (watcher != null) {
      watcher.destroy();
      watcher = null;
    }
    if (socket != null) {
      socket.close();
      socket = null;
    }
    if (portFile != null) {
      try {
        Files.deleteIfExists(portFile);
      } catch (IOException e) {
        // shutting down, ignore
      }
      portFile = null;
    }
  }
}
