package com.webforj.bbjship.cli.admin;

import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.BBjAdminException;
import com.webforj.bbjship.model.BBjShipVersion;
import com.webforj.plugin.foundation.bbj.model.BBjCredentials;
import java.io.IOException;
import java.lang.ProcessBuilder.Redirect;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.OptionalInt;
import java.util.OptionalLong;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;
import java.util.function.IntPredicate;
import java.util.function.LongFunction;

/**
 * Starts and restarts the BBjServices of the BBj installation on this machine.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class BBjShipServices {

  private static final String SERVICE = "BBjServices";
  private static final int ADMIN_PORT = 2002;
  private static final int PROBE_TIMEOUT_MILLIS = 1000;
  private static final Duration STOP_TIMEOUT = Duration.ofSeconds(180);
  private static final Duration POLL_INTERVAL = Duration.ofSeconds(1);
  private static final Duration PROGRAM_TIMEOUT = Duration.ofSeconds(120);
  private static final Duration PROGRESS_INTERVAL = Duration.ofSeconds(10);
  private static final List<String> SERVICE_QUERY = List.of("sc", "query", SERVICE);
  private static final List<String> SERVICE_START = List.of("net", "start", SERVICE);

  private final BBjShipPlugin plugin;
  private String operatingSystem = System.getProperty("os.name");
  private IntPredicate portProbe = BBjShipServices::isPortOpen;
  private LongFunction<Optional<ProcessHandle>> processLookup = ProcessHandle::of;
  private ProgramLauncher programLauncher = BBjShipServices::startProcess;
  private Duration stopTimeout = STOP_TIMEOUT;
  private Duration pollInterval = POLL_INTERVAL;
  private Duration programTimeout = PROGRAM_TIMEOUT;
  private Duration progressInterval = PROGRESS_INTERVAL;
  private Consumer<String> log = line -> {
  };

  /**
   * Creates the BBjServices of a BBj installation.
   *
   * @param plugin the BBjShip plugin of the BBj installation
   */
  public BBjShipServices(BBjShipPlugin plugin) {
    this.plugin = plugin;
  }

  /**
   * Shuts BBjServices down, waits until its process has ended and none of its ports accepts a
   * connection, and starts it again with the program the BBj installation ships.
   *
   * <p>
   * The ports are the administration port and the ports of the BBjShip context. The administration
   * session is released once the shutdown is requested.
   * </p>
   *
   * @param admin the administration session
   * @param processId the process id of BBjServices, or empty when it is not known
   *
   * @throws IOException if the start program is missing, the BBjShip context belongs to another
   *         application, BBjServices does not stop or start in time, the start program fails or the
   *         restart is interrupted
   * @throws BBjAdminException if BBjServices refuses the request of the BBjShip context or the
   *         shutdown
   */
  public void restart(BBjAdminBase admin, OptionalLong processId)
      throws IOException, BBjAdminException {
    try {
      List<String> command;
      List<Integer> ports = new ArrayList<>();
      try {
        command = resolveStartCommand();
        ports.add(ADMIN_PORT);
        ports.addAll(new BBjShipContexts(admin, plugin).getPorts());
        admin.shutdownBBjServices(false);
      } finally {
        admin.release();
      }

      long start = System.nanoTime();
      long deadline = start + stopTimeout.toNanos();
      long interval = progressInterval.toNanos();
      long report = interval;
      Optional<ProcessHandle> process =
          processId.isPresent() ? processLookup.apply(processId.getAsLong()) : Optional.empty();
      while (process.filter(ProcessHandle::isAlive).isPresent()
          || ports.stream().anyMatch(portProbe::test)) {
        if (System.nanoTime() - deadline >= 0) {
          throw new IOException(
              "BBjServices did not stop within " + stopTimeout.toSeconds() + " seconds");
        }

        Thread.sleep(pollInterval);
        long elapsed = System.nanoTime() - start;
        if (elapsed >= report) {
          log.accept("Waiting for BBjServices to stop, " + TimeUnit.NANOSECONDS.toSeconds(elapsed)
              + " seconds");
          report = elapsed - elapsed % interval + interval;
        }
      }

      log.accept("BBjServices stopped after "
          + TimeUnit.NANOSECONDS.toSeconds(System.nanoTime() - start) + " seconds");
      startServices(command);
    } catch (InterruptedException interruption) {
      Thread.currentThread().interrupt();
      throw new IOException("BBjServices restart was interrupted", interruption);
    }
  }

  /**
   * Starts BBjServices with the program the BBj installation ships.
   *
   * @throws IOException if the start program is missing, BBjServices does not start in time, the
   *         start program fails or the start is interrupted
   */
  public void start() throws IOException {
    try {
      startServices(resolveStartCommand());
    } catch (InterruptedException interruption) {
      Thread.currentThread().interrupt();
      throw new IOException("BBjServices start was interrupted", interruption);
    }
  }

  /**
   * Indicates whether BBjServices runs, because its administration port accepts a connection.
   *
   * @return true when BBjServices runs
   */
  public boolean isRunning() {
    return portProbe.test(ADMIN_PORT);
  }

  /**
   * Reads the process id of BBjServices from the version answer of BBjShip.
   *
   * @param admin the administration session
   * @param credentials the administration credentials of the session
   *
   * @return the process id, or empty when there is no BBjShip context or BBjShip does not answer
   *         with one
   * @throws IOException if the BBjShip context belongs to another application
   * @throws BBjAdminException if BBjServices refuses the request
   * @throws InterruptedException if the thread is interrupted while waiting for the answer
   */
  public OptionalLong readProcessId(BBjAdminBase admin, BBjCredentials credentials)
      throws IOException, BBjAdminException, InterruptedException {
    BBjShipContexts contexts = new BBjShipContexts(admin, plugin);
    List<Integer> ports = contexts.getPorts();
    if (ports.isEmpty()) {
      return OptionalLong.empty();
    }

    return contexts.fetchVersion(ports.get(0), credentials).stream()
        .mapToLong(BBjShipVersion::getPid).filter(pid -> pid > 0).findFirst();
  }

  /**
   * Sets where the wait for BBjServices to stop is reported.
   *
   * @param log the log sink
   *
   * @throws NullPointerException if log is null
   */
  public void setLog(Consumer<String> log) {
    this.log = Objects.requireNonNull(log);
  }

  void setOperatingSystem(String operatingSystem) {
    this.operatingSystem = operatingSystem;
  }

  void setPortProbe(IntPredicate portProbe) {
    this.portProbe = portProbe;
  }

  void setProcessLookup(LongFunction<Optional<ProcessHandle>> processLookup) {
    this.processLookup = processLookup;
  }

  void setProgramLauncher(ProgramLauncher programLauncher) {
    this.programLauncher = programLauncher;
  }

  void setStopTimeout(Duration stopTimeout) {
    this.stopTimeout = stopTimeout;
  }

  void setPollInterval(Duration pollInterval) {
    this.pollInterval = pollInterval;
  }

  void setProgramTimeout(Duration programTimeout) {
    this.programTimeout = programTimeout;
  }

  void setProgressInterval(Duration progressInterval) {
    this.progressInterval = progressInterval;
  }

  private List<String> resolveStartCommand() throws IOException, InterruptedException {
    Path home = plugin.getHome();
    Path bin = home.resolve("bin");
    if (!operatingSystem.startsWith("Windows")) {
      return List.of(readProgram(bin.resolve("bbjservices")));
    }

    if (programLauncher.start(SERVICE_QUERY, home, programTimeout).equals(OptionalInt.of(0))) {
      return SERVICE_START;
    }

    return List.of(readProgram(bin.resolve("BBjServices.exe")), "-noservice");
  }

  private void startServices(List<String> command) throws IOException, InterruptedException {
    OptionalInt exit = programLauncher.start(command, plugin.getHome(), programTimeout);
    if (exit.isEmpty()) {
      throw new IOException(
          "BBjServices did not start within " + programTimeout.toSeconds() + " seconds");
    }

    if (exit.getAsInt() != 0) {
      throw new IOException(
          "BBjServices did not start. The program ended with exit code " + exit.getAsInt());
    }
  }

  private static String readProgram(Path program) throws IOException {
    if (!Files.isRegularFile(program)) {
      throw new IOException("BBjServices start program is missing: " + program);
    }

    return program.toString();
  }

  private static boolean isPortOpen(int port) {
    try (Socket socket = new Socket()) {
      socket.connect(new InetSocketAddress("localhost", port), PROBE_TIMEOUT_MILLIS);
      return true;
    } catch (IOException refused) {
      return false;
    }
  }

  private static OptionalInt startProcess(List<String> command, Path directory, Duration timeout)
      throws IOException, InterruptedException {
    Process process = new ProcessBuilder(command).directory(directory.toFile())
        .redirectOutput(Redirect.DISCARD).redirectError(Redirect.DISCARD).start();
    if (!process.waitFor(timeout.toMillis(), TimeUnit.MILLISECONDS)) {
      return OptionalInt.empty();
    }

    return OptionalInt.of(process.exitValue());
  }
}
