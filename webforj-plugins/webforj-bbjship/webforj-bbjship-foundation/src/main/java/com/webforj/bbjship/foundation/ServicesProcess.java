package com.webforj.bbjship.foundation;

import com.webforj.bbjship.foundation.PollingWait.Sleeper;
import java.io.IOException;
import java.lang.ProcessBuilder.Redirect;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.OptionalInt;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.function.IntPredicate;
import java.util.function.LongSupplier;

/**
 * Starts the BBjServices of the BBj installation on this machine again after it was asked to shut
 * down, and waits until BBjShip answers.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class ServicesProcess {

  private static final String SERVICE = "BBjServices";
  private static final String RESTART = "BBjServices restart";
  private static final String ANSWER = "BBjShip answer";
  private static final String SECONDS = " seconds";
  private static final int SERVICE_STOPPING_EXIT = 2;
  private static final int ADMIN_PORT = 2002;
  private static final int PROBE_TIMEOUT_MILLIS = 1000;
  private static final Duration STOP_TIMEOUT = Duration.ofSeconds(180);
  private static final Duration POLL_INTERVAL = Duration.ofSeconds(1);
  private static final Duration PROGRAM_TIMEOUT = Duration.ofSeconds(120);
  private static final Duration ANSWER_TIMEOUT = Duration.ofSeconds(120);
  private static final Duration PROGRESS_INTERVAL = Duration.ofSeconds(10);
  private static final List<String> SERVICE_QUERY = List.of("sc", "query", SERVICE);
  private static final List<String> SERVICE_START = List.of("net", "start", SERVICE);

  private final BBjShipPlugin plugin;
  private String operatingSystem = System.getProperty("os.name");
  private IntPredicate portProbe = ServicesProcess::isPortOpen;
  private ProgramLauncher programLauncher = ProgramLauncher.OPERATING_SYSTEM;
  private Duration stopTimeout = STOP_TIMEOUT;
  private Duration pollInterval = POLL_INTERVAL;
  private Duration programTimeout = PROGRAM_TIMEOUT;
  private Duration answerTimeout = ANSWER_TIMEOUT;
  private Duration progressInterval = PROGRESS_INTERVAL;
  private Consumer<String> log = line -> {
  };
  private LongSupplier clock = System::nanoTime;
  private Sleeper sleeper = Thread::sleep;

  /**
   * Creates the BBjServices of a BBj installation.
   *
   * @param plugin the BBjShip plugin of the BBj installation
   */
  public ServicesProcess(BBjShipPlugin plugin) {
    this.plugin = plugin;
  }

  /**
   * Waits until BBjServices, asked to shut down, has stopped, because its administration port
   * refuses connections, starts it again with the program the BBj installation ships, waits until
   * the administration port accepts a connection and then until BBjShip answers its version.
   *
   * @param bbjShip the client of the BBjShip endpoint
   *
   * @throws IOException if the start program is missing, BBjServices does not stop or start in
   *         time, the start program fails, BBjShip does not answer in time or the restart is
   *         interrupted
   */
  public void applyRestart(BBjShipClient bbjShip) throws IOException {
    try {
      Optional<Duration> stopped =
          createWait(RESTART, stopTimeout, "Waiting for BBjServices to stop, ")
              .apply(() -> !isRunning());
      if (stopped.isEmpty()) {
        throw new IOException(
            "BBjServices did not stop within " + stopTimeout.toSeconds() + SECONDS);
      }

      log.accept("BBjServices stopped after " + stopped.get().toSeconds() + SECONDS);
      startServices(resolveStartCommand());
      Optional<Duration> answered =
          createWait(ANSWER, answerTimeout, "Waiting for BBjShip to answer, ")
              .apply(() -> isAnswering(bbjShip));
      if (answered.isEmpty()) {
        throw new IOException(
            "BBjShip did not answer within " + answerTimeout.toSeconds() + SECONDS);
      }
    } catch (InterruptedException interruption) {
      Thread.currentThread().interrupt();
      throw new IOException("BBjServices restart was interrupted", interruption);
    }
  }

  /**
   * Refuses a BBj installation without the program that starts BBjServices.
   *
   * @throws IOException if the start program is missing or the check is interrupted
   */
  public void applyStartValidation() throws IOException {
    try {
      resolveStartCommand();
    } catch (InterruptedException interruption) {
      Thread.currentThread().interrupt();
      throw new IOException("BBjServices start check was interrupted", interruption);
    }
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

  boolean isRunning() {
    return portProbe.test(ADMIN_PORT);
  }

  void setOperatingSystem(String operatingSystem) {
    this.operatingSystem = operatingSystem;
  }

  void setPortProbe(IntPredicate portProbe) {
    this.portProbe = portProbe;
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

  void setAnswerTimeout(Duration answerTimeout) {
    this.answerTimeout = answerTimeout;
  }

  void setProgressInterval(Duration progressInterval) {
    this.progressInterval = progressInterval;
  }

  void setClock(LongSupplier clock) {
    this.clock = clock;
  }

  void setSleeper(Sleeper sleeper) {
    this.sleeper = sleeper;
  }

  private List<String> resolveStartCommand() throws IOException, InterruptedException {
    Path home = plugin.getHome();
    Path bin = home.resolve("bin");
    if (!operatingSystem.startsWith("Windows")) {
      return List.of(readProgram(bin.resolve("bbjservices")));
    }

    if (runProgram(SERVICE_QUERY).equals(OptionalInt.of(0))) {
      return SERVICE_START;
    }

    return List.of(readProgram(bin.resolve("BBjServices.exe")), "-noservice");
  }

  private OptionalInt runProgram(List<String> command) throws IOException, InterruptedException {
    Process process = programLauncher.start(command, plugin.getHome(), Redirect.DISCARD);
    if (!process.waitFor(programTimeout.toMillis(), TimeUnit.MILLISECONDS)) {
      return OptionalInt.empty();
    }

    return OptionalInt.of(process.exitValue());
  }

  private void startServices(List<String> command) throws IOException {
    // The start script of a Unix installation ends once BBjServices runs in the background, the
    // Windows program outside the service keeps running in the foreground. Either way BBjServices
    // has started once its administration port accepts a connection, and a program that ends
    // with an error before that has failed
    AtomicReference<Process> process =
        new AtomicReference<>(programLauncher.start(command, plugin.getHome(), Redirect.DISCARD));
    Optional<Duration> started =
        createWait(RESTART, programTimeout, "Waiting for BBjServices to start, ").apply(() -> {
          if (isRunning()) {
            return true;
          }

          Process program = process.get();
          if (program.isAlive()) {
            return false;
          }

          int exit = program.exitValue();
          if (exit == SERVICE_STOPPING_EXIT && command.equals(SERVICE_START)) {
            // The service control manager answers 2 while the service is still stopping
            process.set(programLauncher.start(command, plugin.getHome(), Redirect.DISCARD));
          } else if (exit != 0) {
            throw new IOException(
                "BBjServices did not start. The program ended with exit code " + exit);
          }

          return false;
        });
    if (started.isEmpty()) {
      throw new IOException(
          "BBjServices did not start within " + programTimeout.toSeconds() + SECONDS);
    }

    log.accept("BBjServices started after " + started.get().toSeconds() + SECONDS);
  }

  private PollingWait createWait(String subject, Duration timeout, String progressMessage) {
    return PollingWait.create(subject, timeout, pollInterval).setLog(log)
        .setProgressMessage(progressMessage).setProgressInterval(progressInterval).setClock(clock)
        .setSleeper(sleeper).build();
  }

  private static boolean isAnswering(BBjShipClient bbjShip) {
    try {
      bbjShip.getVersion();
      return true;
    } catch (IOException unanswered) {
      return false;
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

  /**
   * Starts a program of the operating system.
   *
   * @author Hyyan Abo Fakher
   * @since 26.03
   */
  @FunctionalInterface
  interface ProgramLauncher {

    /**
     * Starts the program as a process of the operating system.
     */
    ProgramLauncher OPERATING_SYSTEM = (command, directory, output) -> new ProcessBuilder(command)
        .directory(directory.toFile()).redirectOutput(output).redirectError(output).start();

    /**
     * Starts the program.
     *
     * @param command the program and its arguments
     * @param directory the working directory of the program
     * @param output where the standard output and the standard error of the program go
     *
     * @return the started program
     * @throws IOException if the program cannot be started
     */
    Process start(List<String> command, Path directory, Redirect output) throws IOException;
  }
}
