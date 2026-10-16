package com.webforj.bbjship.cli.option;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.BBjAdminException;
import com.webforj.bbjship.cli.BBjShipCli;
import com.webforj.bbjship.cli.TerminalLines;
import com.webforj.bbjship.cli.command.BBjServices;
import com.webforj.bbjship.cli.command.Home;
import com.webforj.bbjship.foundation.BBjShipPlugin;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.net.URISyntaxException;
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
import org.junit.jupiter.params.provider.ValueSource;
import picocli.CommandLine.Help.Ansi;

class CredentialOptionsTest {

  private static final String FACTORY_USERNAME = "admin";
  private static final String FACTORY_PASSWORD = "admin123";
  private static final String USERNAME_VARIABLE = "WEBFORJ_BBJ_USERNAME";
  private static final String PASSWORD_VARIABLE = "WEBFORJ_BBJ_PASSWORD";
  private static final String OPERATOR_USERNAME = "operator";
  private static final String ENVIRONMENT_PASSWORD = "environment-secret";
  private static final String ADMINISTRATOR_USERNAME = "administrator";
  private static final String OPTION_PASSWORD = "option-secret";

  @TempDir
  Path temporary;

  private Path home;
  private ByteArrayOutputStream output;
  private BBjAdminBase admin;
  private List<String> credentials;
  private BBjShipCli cli;

  @BeforeEach
  void initCliOnTheBBjHomeFixture() throws IOException, URISyntaxException, BBjAdminException {
    home = Home.createCopy(temporary.resolve("bbx"));
    output = new ByteArrayOutputStream();
    BBjServices services = new BBjServices();
    services.addBBjShipConfiguration(home);
    admin = services.getAdmin();
    credentials = new ArrayList<>();
    Writer stream = new OutputStreamWriter(output, StandardCharsets.UTF_8);
    cli = BBjShipCli.create(new BBjShipPlugin(home)).setTerminal(new TerminalLines(stream))
        .setAnsi(Ansi.OFF).setSessions(login -> {
          credentials.add(login.getUsername());
          credentials.add(login.getPassword());
          return admin;
        }).build();
  }

  @Test
  void shouldUseTheDefaultCredentialsWithoutOptionsOrEnvironment() {
    int status = cli.run("apps", "list");

    assertEquals(0, status);
    assertEquals(List.of(FACTORY_USERNAME, FACTORY_PASSWORD), credentials);
    assertEquals("""
        Warning: the BBj factory administrator account admin is in use with its factory password. \
        Change the password in Enterprise Manager and configure the new credentials
        No applications deployed
        """, output.toString(StandardCharsets.UTF_8));
  }

  @ParameterizedTest
  @MethodSource("getAccounts")
  void shouldWarnOnlyWhileTheFactoryAccountIsInUse(String username, String password,
      String expected) {
    Writer stream = new OutputStreamWriter(output, StandardCharsets.UTF_8);
    BBjShipCli configured = BBjShipCli.create(new BBjShipPlugin(home))
        .setTerminal(new TerminalLines(stream)
            .setVariables(Map.of(USERNAME_VARIABLE, username, PASSWORD_VARIABLE, password)))
        .setAnsi(Ansi.OFF).setSessions(login -> admin).build();

    int status = configured.run("apps", "list");

    assertEquals(0, status);
    assertEquals(expected, output.toString(StandardCharsets.UTF_8));
  }

  @Test
  void shouldShowTheWarningInYellow() {
    Writer stream = new OutputStreamWriter(output, StandardCharsets.UTF_8);
    BBjShipCli colored =
        BBjShipCli.create(new BBjShipPlugin(home)).setTerminal(new TerminalLines(stream))
            .setAnsi(Ansi.ON).setSessions(login -> admin).build();

    int status = colored.run("apps", "list");

    assertEquals(0, status);
    assertEquals("""
        \u001B[33mWarning: the BBj factory administrator account admin is in use with its factory \
        password. Change the password in Enterprise Manager and configure the new credentials\
        \u001B[39m\u001B[0m
        No applications deployed
        """, output.toString(StandardCharsets.UTF_8));
  }

  @ParameterizedTest
  @MethodSource("getIncompleteEnvironments")
  void shouldRefuseIncompleteCredentials(Map<String, String> environment, String message) {
    Writer stream = new OutputStreamWriter(output, StandardCharsets.UTF_8);
    BBjShipCli configured = BBjShipCli.create(new BBjShipPlugin(home))
        .setTerminal(new TerminalLines(stream).setVariables(environment)).setAnsi(Ansi.OFF)
        .setSessions(login -> {
          credentials.add(login.getUsername());
          credentials.add(login.getPassword());
          return admin;
        }).build();

    int status = configured.run("apps", "list");

    assertEquals(2, status);
    assertEquals(message + "\n", output.toString(StandardCharsets.UTF_8));
    assertEquals(List.of(), credentials);
  }

  @Test
  void shouldPreferTheEnvironmentOverTheDefaultCredentials() {
    Writer stream = new OutputStreamWriter(output, StandardCharsets.UTF_8);
    BBjShipCli configured = BBjShipCli.create(new BBjShipPlugin(home))
        .setTerminal(new TerminalLines(stream).setVariables(
            Map.of(USERNAME_VARIABLE, OPERATOR_USERNAME, PASSWORD_VARIABLE, ENVIRONMENT_PASSWORD)))
        .setAnsi(Ansi.OFF).setSessions(login -> {
          credentials.add(login.getUsername());
          credentials.add(login.getPassword());
          return admin;
        }).build();

    int status = configured.run("apps", "list");

    assertEquals(0, status);
    assertEquals(List.of(OPERATOR_USERNAME, ENVIRONMENT_PASSWORD), credentials);
  }

  @Test
  void shouldPreferTheOptionsOverTheEnvironment() {
    Writer stream = new OutputStreamWriter(output, StandardCharsets.UTF_8);
    BBjShipCli configured = BBjShipCli.create(new BBjShipPlugin(home))
        .setTerminal(new TerminalLines(stream).setVariables(
            Map.of(USERNAME_VARIABLE, OPERATOR_USERNAME, PASSWORD_VARIABLE, ENVIRONMENT_PASSWORD))
            .setTyped("stdin-secret"))
        .setAnsi(Ansi.OFF).setSessions(login -> {
          credentials.add(login.getUsername());
          credentials.add(login.getPassword());
          return admin;
        }).build();

    int status =
        configured.run("apps", "list", "--username", ADMINISTRATOR_USERNAME, "--password-stdin");

    assertEquals(0, status);
    assertEquals(List.of(ADMINISTRATOR_USERNAME, "stdin-secret"), credentials);
    assertEquals("""
        Reading the password from standard input
        No applications deployed
        """, output.toString(StandardCharsets.UTF_8));
  }

  @Test
  void shouldPreferThePasswordValueOverTheEnvironment() {
    Writer stream = new OutputStreamWriter(output, StandardCharsets.UTF_8);
    BBjShipCli configured = BBjShipCli.create(new BBjShipPlugin(home))
        .setTerminal(new TerminalLines(stream).setVariables(
            Map.of(USERNAME_VARIABLE, OPERATOR_USERNAME, PASSWORD_VARIABLE, ENVIRONMENT_PASSWORD)))
        .setAnsi(Ansi.OFF).setSessions(login -> {
          credentials.add(login.getUsername());
          credentials.add(login.getPassword());
          return admin;
        }).build();

    int status = configured.run("apps", "list", "--username", ADMINISTRATOR_USERNAME, "--password",
        OPTION_PASSWORD);

    assertEquals(0, status);
    assertEquals(List.of(ADMINISTRATOR_USERNAME, OPTION_PASSWORD), credentials);
    assertEquals("""
        No applications deployed
        """, output.toString(StandardCharsets.UTF_8));
  }

  @Test
  void shouldRefuseThePasswordValueTogetherWithStandardInput() {
    Writer stream = new OutputStreamWriter(output, StandardCharsets.UTF_8);
    TerminalLines terminal = new TerminalLines(stream).setTyped("stdin-secret");
    BBjShipCli configured = BBjShipCli.create(new BBjShipPlugin(home)).setTerminal(terminal)
        .setAnsi(Ansi.OFF).setSessions(login -> {
          credentials.add(login.getUsername());
          credentials.add(login.getPassword());
          return admin;
        }).build();

    int status = configured.run("apps", "list", "--username", ADMINISTRATOR_USERNAME, "--password",
        OPTION_PASSWORD, "--password-stdin");

    assertEquals(2, status);
    assertEquals("""
        Specify only one of --password and --password-stdin
        """, output.toString(StandardCharsets.UTF_8));
    assertEquals(List.of(), credentials);
    assertEquals("stdin-secret", terminal.readLine());
  }

  @ParameterizedTest
  @MethodSource("getMissingPasswordArguments")
  void shouldRefuseMissingPasswordValueWhileParsing(List<String> arguments) {
    int status = cli.run(arguments.toArray(String[]::new));

    assertEquals(2, status);
    assertEquals("""
        Specify a password after --password
        """, output.toString(StandardCharsets.UTF_8));
    assertEquals(List.of(), credentials);
  }

  @Test
  void shouldRefuseStandardInputOnWindows() {
    Writer stream = new OutputStreamWriter(output, StandardCharsets.UTF_8);
    TerminalLines terminal = new TerminalLines(stream).setTyped("stdin-secret");
    BBjShipCli configured = BBjShipCli.create(new BBjShipPlugin(home)).setTerminal(terminal)
        .setOperatingSystem("Windows 11").setAnsi(Ansi.OFF).setSessions(login -> {
          credentials.add(login.getUsername());
          credentials.add(login.getPassword());
          return admin;
        }).build();

    int status =
        configured.run("apps", "list", "--username", ADMINISTRATOR_USERNAME, "--password-stdin");

    assertEquals(2, status);
    assertEquals("""
        Standard input is not available on Windows. Use --password <value> or WEBFORJ_BBJ_PASSWORD
        """, output.toString(StandardCharsets.UTF_8));
    assertEquals(List.of(), credentials);
    assertEquals("stdin-secret", terminal.readLine());
  }

  @Test
  void shouldUseThePasswordValueOnWindows() {
    Writer stream = new OutputStreamWriter(output, StandardCharsets.UTF_8);
    BBjShipCli configured =
        BBjShipCli.create(new BBjShipPlugin(home)).setTerminal(new TerminalLines(stream))
            .setOperatingSystem("Windows 11").setAnsi(Ansi.OFF).setSessions(login -> {
              credentials.add(login.getUsername());
              credentials.add(login.getPassword());
              return admin;
            }).build();

    int status = configured.run("apps", "list", "--username", ADMINISTRATOR_USERNAME, "--password",
        OPTION_PASSWORD);

    assertEquals(0, status);
    assertEquals(List.of(ADMINISTRATOR_USERNAME, OPTION_PASSWORD), credentials);
  }

  @ParameterizedTest
  @ValueSource(strings = {"", "\n"})
  void shouldAskForPasswordWhenStandardInputIsEmpty(String input) {
    Writer stream = new OutputStreamWriter(output, StandardCharsets.UTF_8);
    BBjShipCli configured = BBjShipCli.create(new BBjShipPlugin(home))
        .setTerminal(new TerminalLines(stream).setTyped(input.lines().toArray(String[]::new)))
        .setAnsi(Ansi.OFF).setSessions(login -> admin).build();

    int status = configured.run("apps", "list", "--password-stdin");

    assertEquals(2, status);
    assertEquals("""
        Reading the password from standard input
        The BBj administration user name and password are missing. Specify --username and \
        --password <value> or --password-stdin, or set WEBFORJ_BBJ_USERNAME and \
        WEBFORJ_BBJ_PASSWORD
        """, output.toString(StandardCharsets.UTF_8));
  }

  @Test
  void shouldRefuseBlankEnvironmentValues() {
    Writer stream = new OutputStreamWriter(output, StandardCharsets.UTF_8);
    BBjShipCli configured = BBjShipCli.create(new BBjShipPlugin(home))
        .setTerminal(new TerminalLines(stream)
            .setVariables(Map.of(USERNAME_VARIABLE, " ", PASSWORD_VARIABLE, "")))
        .setAnsi(Ansi.OFF).setSessions(login -> {
          credentials.add(login.getUsername());
          credentials.add(login.getPassword());
          return admin;
        }).build();

    int status = configured.run("apps", "list");

    assertEquals(2, status);
    assertEquals(List.of(), credentials);
    assertEquals("The BBj administration user name and password are missing. Specify --username "
        + "and --password <value> or --password-stdin, or set WEBFORJ_BBJ_USERNAME and "
        + "WEBFORJ_BBJ_PASSWORD\n", output.toString(StandardCharsets.UTF_8));
  }

  private static Stream<Arguments> getAccounts() {
    return Stream.of(Arguments.of(FACTORY_USERNAME, FACTORY_PASSWORD, """
        Warning: the BBj factory administrator account admin is in use with its factory password. \
        Change the password in Enterprise Manager and configure the new credentials
        No applications deployed
        """), Arguments.of(FACTORY_USERNAME, "secret", """
        No applications deployed
        """), Arguments.of(OPERATOR_USERNAME, FACTORY_PASSWORD, """
        No applications deployed
        """));
  }

  private static Stream<Arguments> getMissingPasswordArguments() {
    return Stream.of(Arguments.of(List.of("apps", "list", "--password", "")),
        Arguments.of(List.of("apps", "list", "--password", "--port")));
  }

  private static Stream<Arguments> getIncompleteEnvironments() {
    String supply = ". Specify --username and --password <value> or --password-stdin, or set "
        + "WEBFORJ_BBJ_USERNAME and " + PASSWORD_VARIABLE;
    return Stream.of(
        Arguments.of(Map.of(USERNAME_VARIABLE, OPERATOR_USERNAME),
            "The BBj administration password is missing" + supply),
        Arguments.of(Map.of(PASSWORD_VARIABLE, "secret"),
            "The BBj administration user name is missing" + supply),
        Arguments.of(Map.of(USERNAME_VARIABLE, OPERATOR_USERNAME, PASSWORD_VARIABLE, " "),
            "The BBj administration password is missing" + supply));
  }
}
