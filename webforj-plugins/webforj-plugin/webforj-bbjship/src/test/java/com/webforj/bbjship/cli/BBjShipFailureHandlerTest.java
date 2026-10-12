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
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import picocli.CommandLine.Help.Ansi;

class BBjShipFailureHandlerTest {

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
  @MethodSource("invalidArguments")
  void shouldRefuseInvalidArgumentsWithTheirMessage(List<String> arguments, String message) {
    int status = cli.run(arguments.toArray(String[]::new));

    assertEquals(2, status);
    assertEquals(message + "\n", output.toString(StandardCharsets.UTF_8));
    assertTrue(credentials.isEmpty());
  }

  @Test
  void shouldReportRefusedPassword() {
    PrintStream stream = new PrintStream(output, true, StandardCharsets.UTF_8);
    BBjShipCli refusing = BBjShipCli.create(home).setInput(InputStream.nullInputStream())
        .setEnvironment(Map.of()).setOutput(stream).setError(stream).setAnsi(Ansi.OFF)
        .setConnector((username, password) -> {
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
    PrintStream stream = new PrintStream(output, true, StandardCharsets.UTF_8);
    BBjShipCli failing = BBjShipCli.create(home).setInput(InputStream.nullInputStream())
        .setEnvironment(Map.of()).setOutput(stream).setError(stream).setAnsi(Ansi.OFF)
        .setConnector((username, password) -> {
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
        Arguments.of(List.of("remote", "enable", "extra", "--port", "8888"),
            "Unknown command remote enable extra. Use --help for available commands"),
        Arguments.of(List.of("--all"),
            "Option --all is not available for bbjship. Use --help for available options"),
        Arguments.of(List.of("remote", "start"),
            "Unknown command remote start. Use --help for available commands"),
        Arguments.of(List.of("remote", "enable", "--port", "8888", "--force"),
            "Unknown option --force. Use --help for available options"),
        Arguments.of(List.of("remote", "enable", "--port", "8888", "--all"),
            "Option --all is not available for remote enable. Use --help for available options"),
        Arguments.of(List.of("remote", "disable", "--port", "8888"),
            "Option --port is not available for remote disable. Use --help for available options"),
        Arguments.of(List.of("apps", "list", "--port", "8888"),
            "Option --port is not available for apps list. Use --help for available options"),
        Arguments.of(
            List.of("remote", "enable", "--port", "8888", "--username", "--password-stdin"),
            "Specify a user name after --username"),
        Arguments.of(List.of("remote", "enable", "--port", "8888", "--username", ""),
            "Specify a user name after --username"),
        Arguments.of(List.of("remote", "enable", "--port", "8888", "--port", "8889"),
            "Specify --port only once"),
        Arguments.of(List.of("apps", "deploy"),
            "Specify the path of a deployment archive ending in .zip"),
        Arguments.of(List.of("apps", "undeploy"), "Specify an application name or --all"),
        Arguments.of(List.of("apps", "undeploy", "hello-app", "--all"),
            "Specify an application name or --all"),
        Arguments.of(List.of("apps", "undeploy", "hello-app", "other-app"),
            "Specify one application name"));
  }
}
