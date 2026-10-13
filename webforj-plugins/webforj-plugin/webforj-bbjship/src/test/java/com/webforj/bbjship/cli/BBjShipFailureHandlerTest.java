package com.webforj.bbjship.cli;

import static com.webforj.plugin.foundation.bbj.model.BBjCredentials.FACTORY_ACCOUNT_WARNING;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.BBjAdminException;
import com.basis.api.admin.rest.adapter.BBjAdminAdapterList;
import com.webforj.bbjship.cli.command.Home;
import com.webforj.plugin.foundation.bbj.BBjShipPlugin;
import java.io.ByteArrayOutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import picocli.CommandLine.Help.Ansi;

class BBjShipFailureHandlerTest {

  private static final String PORT_OPTION = "--port";
  private static final String REMOTE_COMMAND = "remote";
  private static final String ENABLE_COMMAND = "enable";
  private static final String ALL_OPTION = "--all";
  private static final String UNDEPLOY_COMMAND = "undeploy";

  @TempDir
  Path temporary;

  private Path home;
  private ByteArrayOutputStream output;
  private List<String> credentials;
  private BBjShipCli cli;

  @BeforeEach
  void initCliOnTheBBjHomeFixture() throws Exception {
    home = Home.createCopy(temporary.resolve("bbx"));
    output = new ByteArrayOutputStream();
    BBjAdminBase admin = mock(BBjAdminBase.class);
    when(admin.getClasspathNames()).thenReturn(new BBjAdminAdapterList<>(List.of()));
    credentials = new ArrayList<>();
    Writer stream = new OutputStreamWriter(output, StandardCharsets.UTF_8);
    cli = BBjShipCli.create(new BBjShipPlugin(home)).setTerminal(new TerminalLines(stream))
        .setAnsi(Ansi.OFF).setSessions(login -> {
          credentials.add(login.getUsername());
          credentials.add(login.getPassword());
          return admin;
        }).build();
  }

  @ParameterizedTest
  @MethodSource("invalidArguments")
  void shouldRefuseInvalidArgumentsWithTheirMessage(List<String> arguments, String message) {
    int status = cli.run(arguments.toArray(String[]::new));

    assertEquals(2, status);
    assertEquals(message + "\n", output.toString(StandardCharsets.UTF_8));
    assertTrue(credentials.isEmpty());
  }

  @Test
  void shouldReportRefusedPassword() {
    Writer stream = new OutputStreamWriter(output, StandardCharsets.UTF_8);
    BBjShipCli refusing = BBjShipCli.create(new BBjShipPlugin(home))
        .setTerminal(new TerminalLines(stream)).setAnsi(Ansi.OFF).setSessions(login -> {
          throw new BBjAdminException("Server failed login: Authentication failed for user"
              + " 'admin'. Invalid username/password.");
        }).build();

    int status = refusing.run("apps", "list");

    assertEquals(1, status);
    assertEquals(FACTORY_ACCOUNT_WARNING + "\n"
        + "Command failed. Server failed login: Authentication failed for user 'admin'."
        + " Invalid username/password.\n", output.toString(StandardCharsets.UTF_8));
  }

  @Test
  void shouldNameFailureThatCarriesNoMessage() {
    Writer stream = new OutputStreamWriter(output, StandardCharsets.UTF_8);
    BBjShipCli failing = BBjShipCli.create(new BBjShipPlugin(home))
        .setTerminal(new TerminalLines(stream)).setAnsi(Ansi.OFF).setSessions(login -> {
          throw new IllegalStateException();
        }).build();

    int status = failing.run("apps", "list");

    assertEquals(1, status);
    assertEquals(
        FACTORY_ACCOUNT_WARNING + "\n" + "Command failed. java.lang.IllegalStateException\n",
        output.toString(StandardCharsets.UTF_8));
  }

  private static Stream<Arguments> invalidArguments() {
    return Stream.of(
        Arguments.of(List.of("deploy"),
            "Unknown command deploy. Use --help for available commands"),
        Arguments.of(List.of(REMOTE_COMMAND, ENABLE_COMMAND, "extra", PORT_OPTION, "8888"),
            "Unknown command remote enable extra. Use --help for available commands"),
        Arguments.of(List.of(ALL_OPTION),
            "Option --all is not available for bbjship. Use --help for available options"),
        Arguments.of(List.of(REMOTE_COMMAND, "start"),
            "Unknown command remote start. Use --help for available commands"),
        Arguments.of(List.of(REMOTE_COMMAND, ENABLE_COMMAND, PORT_OPTION, "8888", "--force"),
            "Unknown option --force. Use --help for available options"),
        Arguments.of(List.of(REMOTE_COMMAND, ENABLE_COMMAND, PORT_OPTION, "8888", ALL_OPTION),
            "Option --all is not available for remote enable. Use --help for available options"),
        Arguments.of(List.of(REMOTE_COMMAND, "disable", PORT_OPTION, "8888"),
            "Option --port is not available for remote disable. Use --help for available options"),
        Arguments.of(List.of("apps", "list", PORT_OPTION, "8888"),
            "Option --port is not available for apps list. Use --help for available options"),
        Arguments.of(List.of(REMOTE_COMMAND, ENABLE_COMMAND, PORT_OPTION, "8888", "--username",
            "--password-stdin"), "Specify a user name after --username"),
        Arguments.of(List.of(REMOTE_COMMAND, ENABLE_COMMAND, PORT_OPTION, "8888", "--username", ""),
            "Specify a user name after --username"),
        Arguments.of(
            List.of(REMOTE_COMMAND, ENABLE_COMMAND, PORT_OPTION, "8888", PORT_OPTION, "8889"),
            "Specify --port only once"),
        Arguments.of(List.of("apps", "deploy"),
            "Specify the path of a deployment archive ending in .zip"),
        Arguments.of(List.of("apps", UNDEPLOY_COMMAND), "Specify an application name or --all"),
        Arguments.of(List.of("apps", UNDEPLOY_COMMAND, "hello-app", ALL_OPTION),
            "Specify an application name or --all"),
        Arguments.of(List.of("apps", UNDEPLOY_COMMAND, "hello-app", "other-app"),
            "Specify one application name"));
  }
}
