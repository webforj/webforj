package com.webforj.bbjship.cli.option;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.basis.api.admin.BBjAdminBase;
import com.webforj.bbjship.cli.BBjShipCli;
import com.webforj.bbjship.cli.command.BBjServices;
import com.webforj.bbjship.cli.command.Home;
import java.io.ByteArrayInputStream;
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
import org.junit.jupiter.params.provider.ValueSource;
import picocli.CommandLine.Help.Ansi;

class CredentialOptionsTest {

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
    BBjServices services = new BBjServices();
    services.addBBjShipConfiguration(home);
    admin = services.getAdmin();
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
  void shouldUseTheDefaultCredentialsWithoutOptionsOrEnvironment() {
    int status = cli.run("apps", "list");

    assertEquals(0, status);
    assertEquals(List.of("admin", "admin123"), credentials);
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
    PrintStream stream = new PrintStream(output, true, StandardCharsets.UTF_8);
    BBjShipCli configured = BBjShipCli.create(home).setInput(InputStream.nullInputStream())
        .setEnvironment(Map.of("WEBFORJ_BBJ_USERNAME", username, "WEBFORJ_BBJ_PASSWORD", password))
        .setOutput(stream).setError(stream).setAnsi(Ansi.OFF).setConnector((name, secret) -> admin)
        .build();

    int status = configured.run("apps", "list");

    assertEquals(0, status);
    assertEquals(expected, output.toString(StandardCharsets.UTF_8));
  }

  @Test
  void shouldShowTheWarningInYellow() {
    PrintStream stream = new PrintStream(output, true, StandardCharsets.UTF_8);
    BBjShipCli colored = BBjShipCli.create(home).setInput(InputStream.nullInputStream())
        .setEnvironment(Map.of()).setOutput(stream).setError(stream).setAnsi(Ansi.ON)
        .setConnector((name, secret) -> admin).build();

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
    PrintStream stream = new PrintStream(output, true, StandardCharsets.UTF_8);
    BBjShipCli configured = BBjShipCli.create(home).setInput(InputStream.nullInputStream())
        .setEnvironment(environment).setOutput(stream).setError(stream).setAnsi(Ansi.OFF)
        .setConnector((username, password) -> {
          credentials.add(username);
          credentials.add(password);
          return admin;
        }).build();

    int status = configured.run("apps", "list");

    assertEquals(2, status);
    assertEquals(message + "\n", output.toString(StandardCharsets.UTF_8));
    assertEquals(List.of(), credentials);
  }

  @Test
  void shouldPreferTheEnvironmentOverTheDefaultCredentials() {
    PrintStream stream = new PrintStream(output, true, StandardCharsets.UTF_8);
    BBjShipCli configured = BBjShipCli.create(home).setInput(InputStream.nullInputStream())
        .setEnvironment(Map.of("WEBFORJ_BBJ_USERNAME", "operator", "WEBFORJ_BBJ_PASSWORD",
            "environment-secret"))
        .setOutput(stream).setError(stream).setAnsi(Ansi.OFF).setConnector((username, password) -> {
          credentials.add(username);
          credentials.add(password);
          return admin;
        }).build();

    int status = configured.run("apps", "list");

    assertEquals(0, status);
    assertEquals(List.of("operator", "environment-secret"), credentials);
  }

  @Test
  void shouldPreferTheOptionsOverTheEnvironment() {
    PrintStream stream = new PrintStream(output, true, StandardCharsets.UTF_8);
    BBjShipCli configured = BBjShipCli.create(home)
        .setInput(new ByteArrayInputStream("stdin-secret\n".getBytes(StandardCharsets.UTF_8)))
        .setEnvironment(Map.of("WEBFORJ_BBJ_USERNAME", "operator", "WEBFORJ_BBJ_PASSWORD",
            "environment-secret"))
        .setOutput(stream).setError(stream).setAnsi(Ansi.OFF).setConnector((username, password) -> {
          credentials.add(username);
          credentials.add(password);
          return admin;
        }).build();

    int status = configured.run("apps", "list", "--username", "administrator", "--password-stdin");

    assertEquals(0, status);
    assertEquals(List.of("administrator", "stdin-secret"), credentials);
    assertEquals("""
        Reading the password from standard input
        No applications deployed
        """, output.toString(StandardCharsets.UTF_8));
  }

  @ParameterizedTest
  @ValueSource(strings = {"", "\n"})
  void shouldAskForPasswordWhenStandardInputIsEmpty(String input) {
    PrintStream stream = new PrintStream(output, true, StandardCharsets.UTF_8);
    BBjShipCli configured = BBjShipCli.create(home)
        .setInput(new ByteArrayInputStream(input.getBytes(StandardCharsets.UTF_8)))
        .setEnvironment(Map.of()).setOutput(stream).setError(stream).setAnsi(Ansi.OFF)
        .setConnector((username, password) -> admin).build();

    int status = configured.run("apps", "list", "--password-stdin");

    assertEquals(2, status);
    assertEquals("""
        Reading the password from standard input
        Specify a password
        """, output.toString(StandardCharsets.UTF_8));
  }

  @Test
  void shouldRefuseBlankEnvironmentValues() {
    PrintStream stream = new PrintStream(output, true, StandardCharsets.UTF_8);
    BBjShipCli configured = BBjShipCli.create(home).setInput(InputStream.nullInputStream())
        .setEnvironment(Map.of("WEBFORJ_BBJ_USERNAME", " ", "WEBFORJ_BBJ_PASSWORD", ""))
        .setOutput(stream).setError(stream).setAnsi(Ansi.OFF).setConnector((username, password) -> {
          credentials.add(username);
          credentials.add(password);
          return admin;
        }).build();

    int status = configured.run("apps", "list");

    assertEquals(2, status);
    assertEquals(List.of(), credentials);
    assertEquals("Specify a user name\n", output.toString(StandardCharsets.UTF_8));
  }

  private static Stream<Arguments> getAccounts() {
    return Stream.of(Arguments.of("admin", "admin123", """
        Warning: the BBj factory administrator account admin is in use with its factory password. \
        Change the password in Enterprise Manager and configure the new credentials
        No applications deployed
        """), Arguments.of("admin", "secret", """
        No applications deployed
        """), Arguments.of("operator", "admin123", """
        No applications deployed
        """));
  }

  private static Stream<Arguments> getIncompleteEnvironments() {
    return Stream.of(
        Arguments.of(Map.of("WEBFORJ_BBJ_USERNAME", "operator"),
            "Specify --password or --password-stdin, or set WEBFORJ_BBJ_PASSWORD"),
        Arguments.of(Map.of("WEBFORJ_BBJ_PASSWORD", "secret"),
            "Specify --username or set WEBFORJ_BBJ_USERNAME"),
        Arguments.of(Map.of("WEBFORJ_BBJ_USERNAME", "operator", "WEBFORJ_BBJ_PASSWORD", " "),
            "Specify a password"));
  }
}
