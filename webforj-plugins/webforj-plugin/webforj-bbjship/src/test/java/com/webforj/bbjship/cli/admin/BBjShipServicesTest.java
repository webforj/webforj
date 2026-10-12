package com.webforj.bbjship.cli.admin;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import com.basis.api.admin.BBjAdminContextInfo;
import com.basis.api.admin.BBjAdminException;
import com.basis.api.admin.rest.adapter.BBjAdminAdapterList;
import com.webforj.plugin.foundation.bbj.model.BBjCredentials;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.OptionalLong;
import java.util.concurrent.Callable;
import java.util.concurrent.TimeUnit;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class BBjShipServicesTest extends EmptyWebAppServer {

  private List<String> events;
  private BBjShipServices services;
  private boolean started;

  @BeforeEach
  void initStoppedServicesOnTheBBjHomeFixture() throws Exception {
    events = new ArrayList<>();
    doAnswer(call -> events.add("shutdown")).when(admin).shutdownBBjServices(false);
    doAnswer(call -> events.add("release")).when(admin).release();
    services = new BBjShipServices(plugin);
    services.setOperatingSystem("Mac OS X");
    services.setPortProbe(port -> started);
    services.setProcessLookup(pid -> {
      events.add("process " + pid);
      return Optional.empty();
    });
    services.setPollInterval(Duration.ofMillis(1));
    services.setStopTimeout(Duration.ofMillis(5));
    services.setProgramTimeout(Duration.ofMillis(5));
    services.setProgramLauncher((command, directory) -> launch(command, endedProcess(0)));
  }

  @Test
  void shouldStartTheScriptInTheHomeOnceBbjServicesIsGone() throws Exception {
    Deque<Boolean> answers = new ArrayDeque<>(List.of(true, true, false, true));
    services.setPortProbe(port -> {
      events.add("probe " + port);
      return answers.pop();
    });
    List<Object> launch = new ArrayList<>();
    services.setProgramLauncher((command, directory) -> {
      launch.add(directory);
      return launch(command, endedProcess(0));
    });

    services.restart(admin, OptionalLong.empty());

    assertEquals(List.of("shutdown", "release", "probe 2002", "probe 2002", "probe 2002",
        home.resolve("bin/bbjservices").toString(), "probe 2002"), events);
    assertEquals(List.of(home), launch);
  }

  @Test
  void shouldStartOnceTheAdministrationPortAndTheBbjShipContextPortsAllRefuse() throws Exception {
    when(server.getContextNames())
        .thenReturn(new BBjAdminAdapterList<>(List.of("root", "webforj-bbjship")));
    when(server.getContextInfo("webforj-bbjship")).thenReturn(info);
    when(info.getString(BBjAdminContextInfo.JSON_DATA)).thenReturn("""
        {"HTTP port": 8888, "SSL port": 8443, "Document Base": "%s"}"""
        .formatted(home.resolve("plugins/.webforj-bbjship-deployments/htdocs")));
    Map<Integer, Deque<Boolean>> answers =
        Map.of(2002, new ArrayDeque<>(List.of(true, false, false, false, true)), 8888,
            new ArrayDeque<>(List.of(true, true, false)), 8443, new ArrayDeque<>(List.of(false)));
    services.setPortProbe(port -> {
      events.add("probe " + port);
      return answers.get(port).pop();
    });
    services.setStopTimeout(Duration.ofMillis(5000));

    services.restart(admin, OptionalLong.empty());

    assertEquals(List.of("shutdown", "release", "probe 2002", "probe 2002", "probe 8888",
        "probe 2002", "probe 8888", "probe 2002", "probe 8888", "probe 8443",
        home.resolve("bin/bbjservices").toString(), "probe 2002"), events);
  }

  @Test
  void shouldProbeThePortsOnceTheProcessOfBbjServicesHasEnded() throws Exception {
    Deque<Boolean> answers = new ArrayDeque<>(List.of(true, true, false));
    ProcessHandle process = mock(ProcessHandle.class);
    when(process.isAlive()).thenAnswer(call -> {
      events.add("alive " + answers.peek());
      return answers.pop();
    });
    services.setProcessLookup(pid -> {
      events.add("process " + pid);
      return Optional.of(process);
    });
    services.setPortProbe(port -> {
      events.add("probe " + port);
      return started;
    });
    services.setStopTimeout(Duration.ofMillis(5000));

    services.restart(admin, OptionalLong.of(42));

    assertEquals(List.of("shutdown", "release", "process 42", "alive true", "alive true",
        "alive false", "probe 2002", home.resolve("bin/bbjservices").toString(), "probe 2002"),
        events);
  }

  @Test
  void shouldWaitForTheAdministrationPortAfterTheStartScriptHasEnded() throws Exception {
    Deque<Boolean> answers = new ArrayDeque<>(List.of(false, false, false, true));
    services.setPortProbe(port -> {
      events.add("probe " + port);
      return answers.pop();
    });
    services.setProgramTimeout(Duration.ofMillis(5000));

    services.restart(admin, OptionalLong.empty());

    assertEquals(List.of("shutdown", "release", "probe 2002",
        home.resolve("bin/bbjservices").toString(), "probe 2002", "probe 2002", "probe 2002"),
        events);
  }

  @Test
  void shouldWaitForTheAdministrationPortWhileTheStartProgramKeepsRunning() throws Exception {
    Deque<Boolean> answers = new ArrayDeque<>(List.of(false, false, true));
    services.setPortProbe(port -> {
      events.add("probe " + port);
      return answers.pop();
    });
    services.setProgramLauncher((command, directory) -> launch(command, runningProcess()));
    services.setProgramTimeout(Duration.ofMillis(5000));

    services.restart(admin, OptionalLong.empty());

    assertEquals(List.of("shutdown", "release", "probe 2002",
        home.resolve("bin/bbjservices").toString(), "probe 2002", "probe 2002"), events);
  }

  @Test
  void shouldReportTheWaitUntilBbjServicesHasStoppedAndStarted() throws Exception {
    Deque<Boolean> answers = new ArrayDeque<>(List.of(true, true, false));
    ProcessHandle process = mock(ProcessHandle.class);
    when(process.isAlive()).thenAnswer(call -> answers.pop());
    services.setProcessLookup(pid -> Optional.of(process));
    Deque<Boolean> ports = new ArrayDeque<>(List.of(false, false, true));
    services.setPortProbe(port -> ports.pop());
    services.setStopTimeout(Duration.ofMillis(5000));
    services.setProgramTimeout(Duration.ofMillis(5000));
    services.setProgressInterval(Duration.ofMillis(1));
    List<String> log = new ArrayList<>();
    services.setLog(log::add);

    services.restart(admin, OptionalLong.of(42));

    assertEquals(
        List.of("Waiting for BBjServices to stop, 0 seconds",
            "Waiting for BBjServices to stop, 0 seconds", "BBjServices stopped after 0 seconds",
            "Waiting for BBjServices to start, 0 seconds", "BBjServices started after 0 seconds"),
        log);
  }

  @Test
  void shouldFailWithoutStartWhenTheProcessOfBbjServicesDoesNotEnd() {
    ProcessHandle process = mock(ProcessHandle.class);
    when(process.isAlive()).thenReturn(true);
    services.setProcessLookup(pid -> Optional.of(process));
    OptionalLong processId = OptionalLong.of(42);

    IOException failure = assertThrows(IOException.class, () -> services.restart(admin, processId));

    assertEquals("BBjServices did not stop within 0 seconds", failure.getMessage());
    assertEquals(List.of("shutdown", "release"), events);
  }

  @Test
  void shouldReadNoProcessIdWithoutBbjShipContext() throws Exception {
    OptionalLong processId =
        services.readProcessId(admin, BBjCredentials.of("administrator", "secret"));

    assertEquals(OptionalLong.empty(), processId);
  }

  @ParameterizedTest
  @MethodSource("provideVersionAnswers")
  void shouldReadTheProcessIdTheBbjShipAnswers(String version, OptionalLong expected)
      throws Exception {
    try (WebServer web =
        WebServer.startServingVersion("Basic YWRtaW5pc3RyYXRvcjpzZWNyZXQ=", version)) {
      when(server.getContextNames())
          .thenReturn(new BBjAdminAdapterList<>(List.of("root", "webforj-bbjship")));
      when(server.getContextInfo("webforj-bbjship")).thenReturn(info);
      when(info.getString(BBjAdminContextInfo.JSON_DATA)).thenReturn("""
          {"HTTP port": %d, "Document Base": "%s"}""".formatted(web.getPort(),
          home.resolve("plugins/.webforj-bbjship-deployments/htdocs")));

      OptionalLong processId =
          services.readProcessId(admin, BBjCredentials.of("administrator", "secret"));

      assertEquals(expected, processId);
    }
  }

  @ParameterizedTest
  @MethodSource("provideBBjShipsWithoutVersion")
  void shouldReadNoProcessIdWhenTheBbjShipDoesNotAnswerWithItsVersion(Callable<WebServer> bbjShip)
      throws Exception {
    try (WebServer web = bbjShip.call()) {
      when(server.getContextNames())
          .thenReturn(new BBjAdminAdapterList<>(List.of("root", "webforj-bbjship")));
      when(server.getContextInfo("webforj-bbjship")).thenReturn(info);
      when(info.getString(BBjAdminContextInfo.JSON_DATA)).thenReturn("""
          {"HTTP port": %d, "Document Base": "%s"}""".formatted(web.getPort(),
          home.resolve("plugins/.webforj-bbjship-deployments/htdocs")));

      OptionalLong processId =
          services.readProcessId(admin, BBjCredentials.of("administrator", "secret"));

      assertEquals(OptionalLong.empty(), processId);
    }
  }

  @Test
  void shouldReadNoProcessIdWhenTheBbjShipPortRefusesTheConnection() throws Exception {
    when(server.getContextNames())
        .thenReturn(new BBjAdminAdapterList<>(List.of("root", "webforj-bbjship")));
    when(server.getContextInfo("webforj-bbjship")).thenReturn(info);
    when(info.getString(BBjAdminContextInfo.JSON_DATA)).thenReturn("""
        {"HTTP port": %d, "Document Base": "%s"}""".formatted(WebServer.findClosedPort(),
        home.resolve("plugins/.webforj-bbjship-deployments/htdocs")));

    OptionalLong processId =
        services.readProcessId(admin, BBjCredentials.of("administrator", "secret"));

    assertEquals(OptionalLong.empty(), processId);
  }

  @Test
  void shouldStartTheProgramWithoutTheSession() throws Exception {
    services.start();

    assertEquals(List.of(home.resolve("bin/bbjservices").toString()), events);
    verifyNoInteractions(admin);
  }

  @Test
  void shouldFailAndKeepTheInterruptWhenTheStartIsInterrupted() {
    services.setProgramLauncher((command, directory) -> launch(command, runningProcess()));
    services.setPortProbe(port -> {
      Thread.currentThread().interrupt();
      return false;
    });

    IOException failure = assertThrows(IOException.class, services::start);

    assertEquals("BBjServices start was interrupted", failure.getMessage());
    assertInstanceOf(InterruptedException.class, failure.getCause());
    assertTrue(Thread.interrupted());
  }

  @Test
  void shouldStartTheWindowsServiceWhenItExists() throws Exception {
    services.setOperatingSystem("Windows 11");

    services.restart(admin, OptionalLong.empty());

    assertEquals(List.of("sc query BBjServices", "shutdown", "release", "net start BBjServices"),
        events);
  }

  @Test
  void shouldStartTheWindowsProgramWithoutServiceWhenNoServiceExists() throws Exception {
    services.setOperatingSystem("Windows 11");
    services.setProgramLauncher((command, directory) -> launch(command,
        command.get(0).equals("sc") ? endedProcess(1060) : runningProcess()));
    Path program = Files.createFile(home.resolve("bin/BBjServices.exe"));

    services.restart(admin, OptionalLong.empty());

    assertEquals(List.of("sc query BBjServices", "shutdown", "release", program + " -noservice"),
        events);
  }

  @Test
  void shouldFailBeforeTheShutdownWhenTheStartScriptIsMissing() throws Exception {
    Path script = home.resolve("bin/bbjservices");
    Files.delete(script);

    IOException failure =
        assertThrows(IOException.class, () -> services.restart(admin, OptionalLong.empty()));

    assertEquals("BBjServices start program is missing: " + script, failure.getMessage());
    assertEquals(List.of("release"), events);
  }

  @Test
  void shouldFailBeforeTheShutdownWhenTheWindowsProgramIsMissing() {
    services.setOperatingSystem("Windows 11");
    services.setProgramLauncher((command, directory) -> launch(command, endedProcess(1060)));

    IOException failure =
        assertThrows(IOException.class, () -> services.restart(admin, OptionalLong.empty()));

    assertEquals("BBjServices start program is missing: " + home.resolve("bin/BBjServices.exe"),
        failure.getMessage());
    assertEquals(List.of("sc query BBjServices", "release"), events);
  }

  @Test
  void shouldStartTheWindowsProgramWhenTheServiceQueryDoesNotEnd() throws Exception {
    services.setOperatingSystem("Windows 11");
    services.setProgramLauncher((command, directory) -> launch(command,
        command.get(0).equals("sc") ? runningProcess() : endedProcess(0)));
    Path program = Files.createFile(home.resolve("bin/BBjServices.exe"));

    services.restart(admin, OptionalLong.empty());

    assertEquals(List.of("sc query BBjServices", "shutdown", "release", program + " -noservice"),
        events);
  }

  @Test
  void shouldReleaseTheSessionWhenBbjServicesRefusesTheShutdown() throws Exception {
    BBjAdminException refusal = new BBjAdminException("Shutdown refused");
    doThrow(refusal).when(admin).shutdownBBjServices(false);

    BBjAdminException failure =
        assertThrows(BBjAdminException.class, () -> services.restart(admin, OptionalLong.empty()));

    assertEquals(refusal, failure);
    assertEquals(List.of("release"), events);
  }

  @Test
  void shouldFailWithoutStartWhenBbjServicesDoesNotStop() {
    services.setPortProbe(port -> true);

    IOException failure =
        assertThrows(IOException.class, () -> services.restart(admin, OptionalLong.empty()));

    assertEquals("BBjServices did not stop within 0 seconds", failure.getMessage());
    assertEquals(List.of("shutdown", "release"), events);
  }

  @ParameterizedTest
  @MethodSource("provideFailedStarts")
  void shouldFailWhenTheStartProgramFails(Process program, String message) {
    services.setProgramLauncher((command, directory) -> program);

    IOException failure =
        assertThrows(IOException.class, () -> services.restart(admin, OptionalLong.empty()));

    assertEquals(message, failure.getMessage());
  }

  @Test
  void shouldFailAndKeepTheInterruptWhenTheRestartIsInterrupted() {
    services.setPortProbe(port -> {
      Thread.currentThread().interrupt();
      return false;
    });

    IOException failure =
        assertThrows(IOException.class, () -> services.restart(admin, OptionalLong.empty()));

    assertEquals("BBjServices restart was interrupted", failure.getMessage());
    assertInstanceOf(InterruptedException.class, failure.getCause());
    assertTrue(Thread.interrupted());
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

  private static Stream<Arguments> provideVersionAnswers() {
    return Stream.of(Arguments.of("""
        {"version": "26.03-SNAPSHOT", "javaArguments": [], "pid": 42}""", OptionalLong.of(42)),
        Arguments.of("""
            {"version": "26.03-SNAPSHOT", "javaArguments": []}""", OptionalLong.empty()));
  }

  private static Stream<Callable<WebServer>> provideBBjShipsWithoutVersion() {
    return Stream.of(WebServer::startServingBBjShip, WebServer::startServingOther);
  }

  private static Stream<Arguments> provideFailedStarts() {
    return Stream.of(
        Arguments.of(endedProcess(3),
            "BBjServices did not start. The program ended with exit code 3"),
        Arguments.of(runningProcess(), "BBjServices did not start within 0 seconds"),
        Arguments.of(endedProcess(0), "BBjServices did not start within 0 seconds"));
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
