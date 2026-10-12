package com.webforj.bbjship.cli.command;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.rest.adapter.BBjAdminAdapterList;
import com.webforj.bbjship.cli.BBjShipCli;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import picocli.CommandLine.Help.Ansi;

class BBjShipCommandGroupTest {

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

  @ParameterizedTest
  @MethodSource("getCommandGroups")
  void shouldShowTheUsageAndAskForCommandWhenNoneIsGiven(String group, String usage) {
    int status = cli.run(group);

    assertEquals(2, status);
    assertEquals(usage, output.toString(StandardCharsets.UTF_8));
  }

  private static Stream<Arguments> getCommandGroups() {
    return Stream.of(Arguments.of("remote", """
        Usage: bbjship remote [-hV] COMMAND
        Manage the BBjShip HTTP endpoint.
          -h, --help      Show this help message and exit.
          -V, --version   Print version information and exit.
        Commands:
          enable   Enable the BBjShip HTTP endpoint.
          disable  Disable the BBjShip HTTP endpoint.
          status   Show the BBjShip HTTP endpoint.
        Specify a command. Use --help for available commands
        """), Arguments.of("apps", """
        Usage: bbjship apps [-hV] COMMAND
        Deploy, list and undeploy the applications BBjShip deployed.
          -h, --help      Show this help message and exit.
          -V, --version   Print version information and exit.
        Commands:
          deploy    Deploy an application archive.
          list      List the applications BBjShip deployed.
          undeploy  Undeploy an application BBjShip deployed, or all with --all.
        Specify a command. Use --help for available commands
        """), Arguments.of("development", """
        Usage: bbjship development [-hV] COMMAND
        Manage development runs on this BBj machine.
          -h, --help      Show this help message and exit.
          -V, --version   Print version information and exit.
        Commands:
          enable   Enable development runs on this BBj machine.
          disable  Disable development runs on this BBj machine.
        Specify a command. Use --help for available commands
        """), Arguments.of("services", """
        Usage: bbjship services [-hV] COMMAND
        Manage the local BBjServices.
          -h, --help      Show this help message and exit.
          -V, --version   Print version information and exit.
        Commands:
          restart  Restart the local BBjServices.
        Specify a command. Use --help for available commands
        """));
  }
}
