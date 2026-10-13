package com.webforj.bbjship;

import com.basis.api.admin.BBjAdminBBjProcess;
import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.BBjAdminException;
import com.webforj.plugin.foundation.bbj.PollingWait;
import java.io.IOException;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;

/**
 * Ends the BBj sessions that run on the classpath of an application.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
final class SessionTermination {

  private static final Duration TERMINATION_TIMEOUT = Duration.ofSeconds(10);
  private static final Duration TERMINATION_INTERVAL = Duration.ofMillis(250);
  private static final Logger LOGGER = System.getLogger(SessionTermination.class.getName());

  private SessionTermination() {}

  /**
   * Terminates every session of a classpath and waits until they are gone. Sessions that are still
   * running after the timeout are logged and left.
   *
   * @param admin the authenticated administration API
   * @param classpathName the classpath name
   *
   * @throws BBjAdminException if BBjServices refuses to list or terminate a session
   * @throws IOException if the wait is interrupted
   */
  static void stopProcesses(BBjAdminBase admin, String classpathName)
      throws BBjAdminException, IOException {
    for (BBjAdminBBjProcess process : getProcesses(admin, classpathName)) {
      process.terminate();
    }

    // Termination is asynchronous. The sessions must be gone before the loader of the classpath is
    // closed, or a session still shutting down fails on its next class load
    boolean ended = PollingWait
        .create("Stopping the sessions of the BBj classpath " + classpathName, TERMINATION_TIMEOUT,
            TERMINATION_INTERVAL)
        .build().apply(() -> getProcesses(admin, classpathName).isEmpty()).isPresent();
    if (!ended) {
      LOGGER.log(Level.WARNING, "Sessions of the BBj classpath " + classpathName
          + " did not end within " + TERMINATION_TIMEOUT.toSeconds() + " seconds");
    }
  }

  private static List<BBjAdminBBjProcess> getProcesses(BBjAdminBase admin, String classpathName)
      throws BBjAdminException {
    List<BBjAdminBBjProcess> processes = new ArrayList<>();
    for (BBjAdminBBjProcess process : admin.getBBjProcesses()) {
      if (classpathName.equals(process.getString(BBjAdminBBjProcess.SSCP_NAME))) {
        processes.add(process);
      }
    }

    return processes;
  }
}
