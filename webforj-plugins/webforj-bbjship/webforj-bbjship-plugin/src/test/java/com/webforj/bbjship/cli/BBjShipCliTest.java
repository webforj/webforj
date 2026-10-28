package com.webforj.bbjship.cli;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.rest.adapter.BBjAdminAdapterList;
import com.webforj.bbjship.cli.command.Home;
import com.webforj.bbjship.foundation.BBjShipPlugin;
import java.io.ByteArrayOutputStream;
import java.io.OutputStreamWriter;
import java.io.StringWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import picocli.CommandLine.Help.Ansi;

class BBjShipCliTest {

  private static final String HELP_OPTION = "--help";
  private static final String REMOTE_COMMAND = "remote";
  private static final String ENABLE_COMMAND = "enable";

  @TempDir
  Path temporary;

  private Path home;
  private ByteArrayOutputStream output;
  private BBjShipCli cli;

  @BeforeEach
  void initCliOnTheBBjHomeFixture() throws Exception {
    home = Home.createCopy(temporary.resolve("bbx"));
    output = new ByteArrayOutputStream();
    BBjAdminBase admin = mock(BBjAdminBase.class);
    when(admin.getClasspathNames()).thenReturn(new BBjAdminAdapterList<>(List.of()));
    Writer stream = new OutputStreamWriter(output, StandardCharsets.UTF_8);
    cli = BBjShipCli.create(new BBjShipPlugin(home)).setTerminal(new TerminalLines(stream))
        .setAnsi(Ansi.OFF).setSessions(login -> admin).build();
  }

  @Test
  void shouldShowTheCommandsInTheUsageHelp() {
    int status = cli.run(HELP_OPTION);

    assertEquals(0, status);
    assertEquals("""
        Usage: bbjship [-hV] COMMAND
        Configure webforJ BBjShip and manage the applications it deployed.
          -h, --help      Show this help message and exit.
          -V, --version   Print version information and exit.
        Commands:
          remote       Manage the BBjShip HTTP endpoint.
          development  Manage development runs on this BBj machine.
          apps         Deploy, list and undeploy the applications BBjShip deployed.
          reset        Undo every change BBjShip made to this BBj machine.

        WEBFORJ_BBJ_USERNAME and WEBFORJ_BBJ_PASSWORD are used when the options are
        absent. Without options and variables, the BBj factory administrator account is
        used.
        """, output.toString(StandardCharsets.UTF_8));
  }

  @Test
  void shouldShowTheOptionsOfOneCommand() {
    int status = cli.run(REMOTE_COMMAND, ENABLE_COMMAND, HELP_OPTION);

    assertEquals(0, status);
    assertEquals("""
        Usage: bbjship remote enable [-hV] [--allow-unencrypted] [--password-stdin]
                                     [--password=PASSWORD] [--port=PORT]
                                     [--ssl-port=PORT] [--username=USER]
        Enable the BBjShip HTTP endpoint.
              --allow-unencrypted   Accept the administration credentials over plain
                                      HTTP from another machine. Without it, a request
                                      from another machine must use the SSL port.
          -h, --help                Show this help message and exit.
              --password=PASSWORD   BBj administrator password.
              --password-stdin      Read the password from the first line of standard
                                      input.
              --port=PORT           HTTP port of the BBjServices web server the BBjShip
                                      contexts listen on, from 1 to 65535, 8888 when
                                      absent.
              --ssl-port=PORT       SSL port of the BBjServices web server the BBjShip
                                      contexts also listen on, from 1 to 65535.
              --username=USER       BBj administrator user name.
          -V, --version             Print version information and exit.
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
    Writer stream = new OutputStreamWriter(output, StandardCharsets.UTF_8);
    BBjShipCli colored = BBjShipCli.create(new BBjShipPlugin(home))
        .setTerminal(new TerminalLines(stream)).setAnsi(Ansi.ON).build();

    int status = colored.run(REMOTE_COMMAND, ENABLE_COMMAND, "--port", "0");

    assertEquals(2, status);
    assertEquals("\u001B[31m\u001B[1mSpecify --port from 1 to 65535\u001B[21m\u001B[39m\u001B[0m\n",
        output.toString(StandardCharsets.UTF_8));
  }

  @ParameterizedTest
  @MethodSource("getTerminalColors")
  void shouldDecideColorsFromTheVariablesOfTheTerminal(Map<String, String> variables,
      String expected) {
    Writer stream = new OutputStreamWriter(output, StandardCharsets.UTF_8);
    BBjShipCli terminalCli = BBjShipCli.create(new BBjShipPlugin(home))
        .setTerminal(new TerminalLines(stream).setVariables(variables)).build();

    int status = terminalCli.run(REMOTE_COMMAND, ENABLE_COMMAND, "--port", "0");

    assertEquals(2, status);
    assertEquals(expected, output.toString(StandardCharsets.UTF_8));
  }

  @Test
  void shouldRunOneCommandOfTheBbjInstallationThatRunsIt() {
    StringWriter printed = new StringWriter();
    System.setProperty("basis.BBjHome", home.toString());
    int status;
    try {
      status = BBjShipCli.run(new String[] {"-V"}, new TerminalLines(printed));
    } finally {
      System.clearProperty("basis.BBjHome");
    }

    assertEquals(0, status);
    assertEquals("webforJ BBjShip 26.03\n", printed.toString());
  }

  @Test
  void shouldWritePlainTextWithoutColors() {
    int status = cli.run(HELP_OPTION);

    assertEquals(0, status);
    assertFalse(output.toString(StandardCharsets.UTF_8).contains("\u001B["));
  }

  private static Stream<Arguments> getTerminalColors() {
    String colored =
        "\u001B[31m\u001B[1mSpecify --port from 1 to 65535\u001B[21m\u001B[39m\u001B[0m\n";
    String plain = "Specify --port from 1 to 65535\n";

    return Stream.of(Arguments.of(Map.of("TERM", "xterm-256color"), colored),
        Arguments.of(Map.of("TERM", "xterm-256color", "NO_COLOR", "1"), plain),
        Arguments.of(Map.of("TERM", "dumb"), plain), Arguments.of(Map.of(), plain));
  }
}
