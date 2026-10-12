package com.webforj.bbjship.cli.command;

import static com.webforj.plugin.foundation.bbj.model.BBjCredentials.FACTORY_ACCOUNT_WARNING;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.api.admin.BBjAdminException;
import com.webforj.bbjship.cli.BBjShipCli;
import com.webforj.bbjship.cli.admin.BBjShipServices;
import com.webforj.plugin.foundation.bbj.model.BBjCredentials;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.Map;
import java.util.OptionalLong;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import picocli.CommandLine.Help.Ansi;

class ServicesRestartCommandTest {

  @TempDir
  Path temporary;

  private Path home;
  private BBjServices bbjServices;
  private BBjShipServices services;
  private ByteArrayOutputStream output;
  private BBjShipCli cli;

  @BeforeEach
  void initCliWithMockedServicesOnTheBBjHomeFixture() throws Exception {
    home = Home.createCopy(temporary.resolve("bbx"));
    bbjServices = new BBjServices();
    bbjServices.addBBjShipConfiguration(home);
    services = mock(BBjShipServices.class);
    when(services.isRunning()).thenReturn(true);
    output = new ByteArrayOutputStream();
    PrintStream stream = new PrintStream(output, true, StandardCharsets.UTF_8);
    cli = BBjShipCli.create(home).setInput(InputStream.nullInputStream()).setEnvironment(Map.of())
        .setOutput(stream).setError(stream).setAnsi(Ansi.OFF)
        .setConnector((username, password) -> bbjServices.getAdmin()).setServices(services).build();
  }

  @Test
  void shouldRestartBbjServicesWithTheAdminOfTheSessionAndConfirm() throws Exception {
    when(services.readProcessId(eq(bbjServices.getAdmin()),
        argThat(BBjCredentials::isFactoryAccount))).thenReturn(OptionalLong.of(42));

    int status = cli.run("services", "restart");

    assertEquals(0, status);
    assertEquals(FACTORY_ACCOUNT_WARNING + "\n" + "Restarted BBjServices\n",
        output.toString(StandardCharsets.UTF_8));
    verify(services).restart(bbjServices.getAdmin(), OptionalLong.of(42));
  }

  @Test
  void shouldStartBbjServicesWithoutSessionWhenItIsNotRunning() throws Exception {
    when(services.isRunning()).thenReturn(false);
    PrintStream stream = new PrintStream(output, true, StandardCharsets.UTF_8);
    BBjShipCli startingCli = BBjShipCli.create(home).setInput(InputStream.nullInputStream())
        .setEnvironment(Map.of()).setOutput(stream).setError(stream).setAnsi(Ansi.OFF)
        .setConnector((username, password) -> {
          throw new BBjAdminException("The connector was called");
        }).setServices(services).build();

    int status = startingCli.run("services", "restart");

    assertEquals(0, status);
    assertEquals("Started BBjServices\n", output.toString(StandardCharsets.UTF_8));
    verify(services).start();
  }

  @Test
  void shouldFailWhenBbjServicesDoesNotStop() throws Exception {
    doThrow(new IOException("BBjServices did not stop within 180 seconds")).when(services)
        .restart(any(), any());

    int status = cli.run("services", "restart");

    assertEquals(1, status);
    assertEquals(
        FACTORY_ACCOUNT_WARNING + "\n"
            + "Command failed. BBjServices did not stop within 180 seconds\n",
        output.toString(StandardCharsets.UTF_8));
  }

  @Test
  void shouldReleaseTheSessionWithoutRestartWhenTheProcessIdCannotBeRead() throws Exception {
    when(services.readProcessId(any(), any()))
        .thenThrow(new IOException("The BBjShip context belongs to another application"));

    int status = cli.run("services", "restart");

    assertEquals(1, status);
    assertEquals(
        FACTORY_ACCOUNT_WARNING + "\n"
            + "Command failed. The BBjShip context belongs to another application\n",
        output.toString(StandardCharsets.UTF_8));
    verify(bbjServices.getAdmin()).release();
    verify(services, never()).restart(any(), any());
  }
}
