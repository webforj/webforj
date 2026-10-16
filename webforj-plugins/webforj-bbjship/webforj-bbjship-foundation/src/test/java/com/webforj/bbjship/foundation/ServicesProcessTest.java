package com.webforj.bbjship.foundation;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.ProcessBuilder.Redirect;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class ServicesProcessTest {

  private static final String START_SCRIPT = "bin/bbjservices";
  private static final String PROBE_EVENT = "probe ";
  private static final String PROBE_EVENT_2002 = "probe 2002";
  private static final String WINDOWS = "Windows 11";
  private static final String SERVICE_QUERY = "sc query BBjServices";
  private static final String START_PROGRAM = "bin/BBjServices.exe";
  private static final String SERVICE_START = "net start BBjServices";
  private static final String START_TIMEOUT_FAILURE = "BBjServices did not start within 0 seconds";

  @TempDir
  Path temporary;

  private Path home;
  private List<String> events;
  private ServicesProcess services;
  private BBjShipClient bbjShip;
  private boolean started;
  private long now;

  @BeforeEach
  void initStoppedServicesOnTheBBjHomeFixture() throws Exception {
    home = Home.createCopy(temporary.resolve("bbx"));
    events = new ArrayList<>();
    Files.createFile(home.resolve(START_SCRIPT));
    bbjShip = mock(BBjShipClient.class);
    services = new ServicesProcess(new BBjShipPlugin(home));
    services.setOperatingSystem("Mac OS X");
    services.setPortProbe(port -> started);
    services.setClock(() -> now);
    services.setSleeper(duration -> {
      if (Thread.interrupted()) {
        throw new InterruptedException();
      }

      now += duration.toNanos();
    });
    services.setPollInterval(Duration.ofMillis(1));
    services.setStopTimeout(Duration.ofMillis(5));
    services.setProgramTimeout(Duration.ofMillis(5));
    services.setAnswerTimeout(Duration.ofMillis(5));
    services.setProgramLauncher((command, directory, output) -> launch(command, endedProcess(0)));
  }

  @Test
  void shouldStartTheScriptInTheHomeOnceBbjServicesIsGone() throws Exception {
    Deque<Boolean> answers = new ArrayDeque<>(List.of(true, true, false, true));
    services.setPortProbe(port -> {
      events.add(PROBE_EVENT + port);
      return answers.pop();
    });
    List<Object> launch = new ArrayList<>();
    services.setProgramLauncher((command, directory, output) -> {
      launch.add(directory);
      launch.add(output);
      return launch(command, endedProcess(0));
    });
    services.setStopTimeout(Duration.ofMillis(5000));

    services.applyRestart(bbjShip);

    assertEquals(List.of(PROBE_EVENT_2002, PROBE_EVENT_2002, PROBE_EVENT_2002,
        home.resolve(START_SCRIPT).toString(), PROBE_EVENT_2002), events);
    assertEquals(List.of(home, Redirect.DISCARD), launch);
  }

  @Test
  void shouldWaitForTheAdministrationPortAfterTheStartScriptHasEnded() throws Exception {
    Deque<Boolean> answers = new ArrayDeque<>(List.of(false, false, false, true));
    services.setPortProbe(port -> {
      events.add(PROBE_EVENT + port);
      return answers.pop();
    });
    services.setProgramTimeout(Duration.ofMillis(5000));

    services.applyRestart(bbjShip);

    assertEquals(List.of(PROBE_EVENT_2002, home.resolve(START_SCRIPT).toString(), PROBE_EVENT_2002,
        PROBE_EVENT_2002, PROBE_EVENT_2002), events);
  }

  @Test
  void shouldWaitForTheAdministrationPortWhileTheStartProgramKeepsRunning() throws Exception {
    Deque<Boolean> answers = new ArrayDeque<>(List.of(false, false, true));
    services.setPortProbe(port -> {
      events.add(PROBE_EVENT + port);
      return answers.pop();
    });
    services.setProgramLauncher((command, directory, output) -> launch(command, runningProcess()));
    services.setProgramTimeout(Duration.ofMillis(5000));

    services.applyRestart(bbjShip);

    assertEquals(List.of(PROBE_EVENT_2002, home.resolve(START_SCRIPT).toString(), PROBE_EVENT_2002,
        PROBE_EVENT_2002), events);
  }

  @Test
  void shouldReportTheWaitUntilBbjServicesHasStoppedAndStarted() throws Exception {
    Deque<Boolean> ports = new ArrayDeque<>(List.of(true, true, false, false, true));
    services.setPortProbe(port -> ports.pop());
    services.setStopTimeout(Duration.ofMillis(5000));
    services.setProgramTimeout(Duration.ofMillis(5000));
    services.setProgressInterval(Duration.ofMillis(1));
    List<String> log = new ArrayList<>();
    services.setLog(log::add);

    services.applyRestart(bbjShip);

    assertEquals(
        List.of("Waiting for BBjServices to stop, 0 seconds",
            "Waiting for BBjServices to stop, 0 seconds", "BBjServices stopped after 0 seconds",
            "Waiting for BBjServices to start, 0 seconds", "BBjServices started after 0 seconds"),
        log);
  }

  @Test
  void shouldStartTheWindowsServiceWhenItExists() throws Exception {
    services.setOperatingSystem(WINDOWS);

    services.applyRestart(bbjShip);

    assertEquals(List.of(SERVICE_QUERY, SERVICE_START), events);
  }

  @Test
  void shouldStartTheWindowsServiceAgainWhileItIsStillStopping() throws Exception {
    services.setOperatingSystem(WINDOWS);
    Deque<Integer> exits = new ArrayDeque<>(List.of(2, 0));
    services.setProgramLauncher((command, directory, output) -> {
      if (command.get(0).equals("sc")) {
        return launch(command, endedProcess(0));
      }

      int exit = exits.pop();
      Process program = launch(command, endedProcess(exit));
      started = exit == 0;

      return program;
    });
    services.setProgramTimeout(Duration.ofMillis(5000));

    services.applyRestart(bbjShip);

    assertEquals(List.of(SERVICE_QUERY, SERVICE_START, SERVICE_START), events);
  }

  @Test
  void shouldFailWhenTheWindowsServiceStaysStopping() {
    services.setOperatingSystem(WINDOWS);
    services.setProgramLauncher((command, directory, output) -> {
      Process program = launch(command, endedProcess(command.get(0).equals("sc") ? 0 : 2));
      started = false;

      return program;
    });

    IOException failure = assertThrows(IOException.class, () -> services.applyRestart(bbjShip));

    assertEquals(START_TIMEOUT_FAILURE, failure.getMessage());
  }

  @Test
  void shouldStartTheWindowsProgramWithoutServiceWhenNoServiceExists() throws Exception {
    services.setOperatingSystem(WINDOWS);
    services.setProgramLauncher((command, directory, output) -> launch(command,
        command.get(0).equals("sc") ? endedProcess(1060) : runningProcess()));
    Path program = Files.createFile(home.resolve(START_PROGRAM));

    services.applyRestart(bbjShip);

    assertEquals(List.of(SERVICE_QUERY, program + " -noservice"), events);
  }

  @Test
  void shouldAcceptTheInstallationWithTheStartScript() {
    assertDoesNotThrow(services::applyStartValidation);
  }

  @Test
  void shouldRefuseTheInstallationWithoutTheStartScript() throws Exception {
    Path script = home.resolve(START_SCRIPT);
    Files.delete(script);

    IOException failure = assertThrows(IOException.class, services::applyStartValidation);

    assertEquals("BBjServices start program is missing: " + script, failure.getMessage());
    assertEquals(List.of(), events);
  }

  @Test
  void shouldRefuseTheWindowsInstallationWithoutTheProgram() {
    services.setOperatingSystem(WINDOWS);
    services
        .setProgramLauncher((command, directory, output) -> launch(command, endedProcess(1060)));

    IOException failure = assertThrows(IOException.class, services::applyStartValidation);

    assertEquals("BBjServices start program is missing: " + home.resolve(START_PROGRAM),
        failure.getMessage());
    assertEquals(List.of(SERVICE_QUERY), events);
  }

  @Test
  void shouldStartTheWindowsProgramWhenTheServiceQueryDoesNotEnd() throws Exception {
    services.setOperatingSystem(WINDOWS);
    services.setProgramLauncher((command, directory, output) -> launch(command,
        command.get(0).equals("sc") ? runningProcess() : endedProcess(0)));
    Path program = Files.createFile(home.resolve(START_PROGRAM));

    services.applyRestart(bbjShip);

    assertEquals(List.of(SERVICE_QUERY, program + " -noservice"), events);
  }

  @Test
  void shouldFailWithoutStartWhenBbjServicesDoesNotStop() {
    services.setPortProbe(port -> true);

    IOException failure = assertThrows(IOException.class, () -> services.applyRestart(bbjShip));

    assertEquals("BBjServices did not stop within 0 seconds", failure.getMessage());
    assertEquals(List.of(), events);
  }

  @ParameterizedTest
  @MethodSource("provideFailedStarts")
  void shouldFailWhenTheStartProgramFails(Process program, String message) {
    services.setProgramLauncher((command, directory, output) -> program);

    IOException failure = assertThrows(IOException.class, () -> services.applyRestart(bbjShip));

    assertEquals(message, failure.getMessage());
  }

  @Test
  void shouldFailAndKeepTheInterruptWhenTheRestartIsInterrupted() {
    services.setPortProbe(port -> {
      Thread.currentThread().interrupt();
      return true;
    });
    services.setStopTimeout(Duration.ofMillis(5000));

    IOException failure = assertThrows(IOException.class, () -> services.applyRestart(bbjShip));

    assertEquals("BBjServices restart was interrupted", failure.getMessage());
    assertInstanceOf(InterruptedException.class, failure.getCause());
    assertTrue(Thread.interrupted());
  }

  @Test
  void shouldEndTheRestartOnceBbjShipAnswers() throws Exception {
    Deque<Boolean> ports = new ArrayDeque<>(List.of(false, true));
    services.setPortProbe(port -> ports.pop());
    services.setProgramTimeout(Duration.ofMillis(5000));
    services.setAnswerTimeout(Duration.ofMillis(5000));
    when(bbjShip.getVersion()).thenThrow(new IOException("refused")).thenReturn(null);

    services.applyRestart(bbjShip);

    verify(bbjShip, times(2)).getVersion();
  }

  @Test
  void shouldFailWhenBbjShipDoesNotAnswerAfterTheStart() throws Exception {
    Deque<Boolean> ports = new ArrayDeque<>(List.of(false, true));
    services.setPortProbe(port -> ports.pop());
    services.setProgramTimeout(Duration.ofMillis(5000));
    when(bbjShip.getVersion()).thenThrow(new IOException("refused"));

    IOException failure = assertThrows(IOException.class, () -> services.applyRestart(bbjShip));

    assertEquals("BBjShip did not answer within 0 seconds", failure.getMessage());
  }

  @Test
  void shouldCountAnAcceptedPortAsRunning() {
    services.setPortProbe(port -> port == 2002);

    assertTrue(services.isRunning());
  }

  @Test
  void shouldNotCountClosedAdministrationPortAsRunning() {
    assertFalse(services.isRunning());
  }

  private Process launch(List<String> command, Process process) {
    events.add(String.join(" ", command));
    started = !command.get(0).equals("sc");

    return process;
  }

  private static Process endedProcess(int exit) {
    return new Program(false, exit);
  }

  private static Process runningProcess() {
    return new Program(true, 0);
  }

  private static Stream<Arguments> provideFailedStarts() {
    return Stream.of(
        Arguments.of(endedProcess(3),
            "BBjServices did not start. The program ended with exit code 3"),
        Arguments.of(runningProcess(), START_TIMEOUT_FAILURE),
        Arguments.of(endedProcess(0), START_TIMEOUT_FAILURE));
  }

  private static final class Program extends Process {

    private final boolean alive;
    private final int exit;

    private Program(boolean alive, int exit) {
      this.alive = alive;
      this.exit = exit;
    }

    @Override
    public OutputStream getOutputStream() {
      return OutputStream.nullOutputStream();
    }

    @Override
    public InputStream getInputStream() {
      return InputStream.nullInputStream();
    }

    @Override
    public InputStream getErrorStream() {
      return InputStream.nullInputStream();
    }

    @Override
    public int waitFor() {
      return exitValue();
    }

    @Override
    public boolean waitFor(long timeout, TimeUnit unit) {
      return !alive;
    }

    @Override
    public int exitValue() {
      if (alive) {
        throw new IllegalThreadStateException();
      }

      return exit;
    }

    @Override
    public boolean isAlive() {
      return alive;
    }

    @Override
    public void destroy() {
      throw new UnsupportedOperationException();
    }
  }
}
