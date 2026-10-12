package com.webforj.bbjship.cli;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.rest.adapter.BBjAdminAdapterList;
import com.webforj.bbjship.cli.command.Home;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import picocli.CommandLine.Help.Ansi;

class BBjShipCliTest {

  @TempDir
  Path temporary;

  private Path home;
  private ByteArrayOutputStream output;
  private BBjAdminBase admin;
  private List<String> credentials;
  private BBjShipCli cli;

  @BeforeEach
  void initCliOnTheBBjHomeFixture() throws Exception {
    home = Home.createCopy(temporary.resolve("bbx"));
    output = new ByteArrayOutputStream();
    admin = mock(BBjAdminBase.class);
    when(admin.getClasspathNames()).thenReturn(new BBjAdminAdapterList<>(List.of()));
    credentials = new ArrayList<>();
    PrintStream stream = new PrintStream(output, true, StandardCharsets.UTF_8);
    cli = BBjShipCli.create(home).setInput(InputStream.nullInputStream()).setEnvironment(Map.of())
        .setOutput(stream).setError(stream).setAnsi(Ansi.OFF).setConnector((username, password) -> {
          credentials.add(username);
          credentials.add(password);
          return admin;
        }).build();
  }

  @Test
  void shouldShowTheCommandsInTheUsageHelp() {
    int status = cli.run("--help");

    assertEquals(0, status);
    assertEquals("""
        Usage: bbjship [-hV] COMMAND
        Configure webforJ BBjShip and manage the applications it deployed.
          -h, --help      Show this help message and exit.
          -V, --version   Print version information and exit.
        Commands:
          remote       Manage the BBjShip HTTP endpoint.
          development  Manage development runs on this BBj machine.
          services     Manage the local BBjServices.
          apps         Deploy, list and undeploy the applications BBjShip deployed.
          reset        Undo every change BBjShip made to this BBj machine.

        WEBFORJ_BBJ_USERNAME and WEBFORJ_BBJ_PASSWORD are used when the options are
        absent. Without options and variables, the BBj factory administrator account is
        used.
        """, output.toString(StandardCharsets.UTF_8));
  }

  @Test
  void shouldShowTheOptionsOfOneCommand() {
    int status = cli.run("remote", "enable", "--help");

    assertEquals(0, status);
    assertEquals("""
        Usage: bbjship remote enable [-hV] [--password] [--password-stdin]
                                     [--port=PORT] [--ssl-port=PORT] [--username=USER]
        Enable the BBjShip HTTP endpoint.
          -h, --help             Show this help message and exit.
              --password         Prompt for the password without showing it.
              --password-stdin   Read the password from the first line of standard
                                   input.
              --port=PORT        HTTP port of the BBjServices web server the BBjShip
                                   contexts listen on, from 1 to 65535, 8888 when
                                   absent.
              --ssl-port=PORT    SSL port of the BBjServices web server the BBjShip
                                   contexts also listen on, from 1 to 65535.
              --username=USER    BBj administrator user name.
          -V, --version          Print version information and exit.
        """, output.toString(StandardCharsets.UTF_8));
  }

  @Test
  void shouldPrintTheVersionOfThePlugin() {
    int status = cli.run("-V");

    assertEquals(0, status);
    assertEquals("webforJ BBjShip 26.03\n", output.toString(StandardCharsets.UTF_8));
  }

  @Test
  void shouldColorTheOutputWhenTheTerminalSupportsIt() {
    PrintStream stream = new PrintStream(output, true, StandardCharsets.UTF_8);
    BBjShipCli colored = BBjShipCli.create(home).setInput(InputStream.nullInputStream())
        .setEnvironment(Map.of()).setOutput(stream).setError(stream).setAnsi(Ansi.ON).build();

    int status = colored.run("remote", "enable", "--port", "0");

    assertEquals(2, status);
    assertEquals("\u001B[31m\u001B[1mSpecify --port from 1 to 65535\u001B[21m\u001B[39m\u001B[0m\n",
        output.toString(StandardCharsets.UTF_8));
  }

  @Test
  void shouldWritePlainTextWithoutColors() {
    int status = cli.run("--help");

    assertEquals(0, status);
    assertFalse(output.toString(StandardCharsets.UTF_8).contains("\u001B["));
  }
}
