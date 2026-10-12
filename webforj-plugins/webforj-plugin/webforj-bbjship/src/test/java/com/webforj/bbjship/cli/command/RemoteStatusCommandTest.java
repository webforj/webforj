package com.webforj.bbjship.cli.command;

import static com.webforj.plugin.foundation.bbj.model.BBjCredentials.FACTORY_ACCOUNT_WARNING;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import com.webforj.bbjship.cli.BBjShipCli;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import picocli.CommandLine.Help.Ansi;

class RemoteStatusCommandTest {

  @TempDir
  Path temporary;

  @Test
  void shouldPrintTheAddressOfTheBbjShipContext() throws Exception {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    BBjServices services = new BBjServices();
    services.addBBjShipConfiguration(home);
    services.addContext("webforj-bbjship", """
        {
          "HTTP port": 8890,
          "Document Base": "%s",
          "BBx Servlets": [{"Class Name": "WebforjBBjDeploymentServlet", "Parameters": []}]
        }""".formatted(home.resolve("plugins/.webforj-bbjship-deployments/htdocs")));
    ByteArrayOutputStream output = new ByteArrayOutputStream();
    PrintStream stream = new PrintStream(output, true, StandardCharsets.UTF_8);
    BBjShipCli cli = BBjShipCli.create(home).setInput(InputStream.nullInputStream())
        .setEnvironment(Map.of()).setOutput(stream).setError(stream).setAnsi(Ansi.OFF)
        .setConnector((username, password) -> services.getAdmin()).build();

    int exitCode = cli.run("remote", "status");

    assertEquals(0, exitCode);
    assertEquals(FACTORY_ACCOUNT_WARNING + "\n" + "Enabled http://localhost:8890/webforj-bbjship\n"
        + "Development runs  disabled\n", output.toString(StandardCharsets.UTF_8));
    verify(services.getContext("webforj-bbjship"), never()).setString(anyString(), anyString());
    verify(services.getAdmin()).release();
  }

  @Test
  void shouldPrintTheHttpAndTheHttpsAddressOfTheBbjShipContextWithSslPort() throws Exception {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    BBjServices services = new BBjServices();
    services.addBBjShipConfiguration(home);
    services.addContext("webforj-bbjship", """
        {
          "HTTP port": 8888,
          "SSL port": 8443,
          "Document Base": "%s"
        }""".formatted(home.resolve("plugins/.webforj-bbjship-deployments/htdocs")));
    ByteArrayOutputStream output = new ByteArrayOutputStream();
    PrintStream stream = new PrintStream(output, true, StandardCharsets.UTF_8);
    BBjShipCli cli = BBjShipCli.create(home).setInput(InputStream.nullInputStream())
        .setEnvironment(
            Map.of("WEBFORJ_BBJ_USERNAME", "administrator", "WEBFORJ_BBJ_PASSWORD", "secret"))
        .setOutput(stream).setError(stream).setAnsi(Ansi.OFF)
        .setConnector((username, password) -> services.getAdmin()).build();

    int exitCode = cli.run("remote", "status");

    assertEquals(0, exitCode);
    assertEquals("""
        Enabled http://localhost:8888/webforj-bbjship
        Enabled https://localhost:8443/webforj-bbjship
        Development runs  disabled
        """, output.toString(StandardCharsets.UTF_8));
  }

  @Test
  void shouldSayWhenTheBbjShipContextDoesNotExist() throws Exception {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    BBjServices services = new BBjServices();
    services.addBBjShipConfiguration(home);
    ByteArrayOutputStream output = new ByteArrayOutputStream();
    PrintStream stream = new PrintStream(output, true, StandardCharsets.UTF_8);
    BBjShipCli cli = BBjShipCli.create(home).setInput(InputStream.nullInputStream())
        .setEnvironment(Map.of()).setOutput(stream).setError(stream).setAnsi(Ansi.OFF)
        .setConnector((username, password) -> services.getAdmin()).build();

    int exitCode = cli.run("remote", "status");

    assertEquals(0, exitCode);
    assertEquals(FACTORY_ACCOUNT_WARNING + "\n" + "The BBjShip HTTP endpoint is not enabled\n"
        + "Development runs  disabled\n", output.toString(StandardCharsets.UTF_8));
    verify(services.getServer(), never()).createContext(anyString());
    verify(services.getAdmin()).release();
  }

  @Test
  void shouldSayWhenDevelopmentRunsAreEnabled() throws Exception {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Path runtime = Files.createDirectories(home.resolve("plugins/.webforj-bbjship-deployments"));
    Files.writeString(runtime.resolve("development.enabled"),
        "development runs are enabled on this machine");
    BBjServices services = new BBjServices();
    services.addBBjShipConfiguration(home);
    ByteArrayOutputStream output = new ByteArrayOutputStream();
    PrintStream stream = new PrintStream(output, true, StandardCharsets.UTF_8);
    BBjShipCli cli = BBjShipCli.create(home).setInput(InputStream.nullInputStream())
        .setEnvironment(Map.of()).setOutput(stream).setError(stream).setAnsi(Ansi.OFF)
        .setConnector((username, password) -> services.getAdmin()).build();

    int exitCode = cli.run("remote", "status");

    assertEquals(0, exitCode);
    assertEquals(FACTORY_ACCOUNT_WARNING + "\n" + "The BBjShip HTTP endpoint is not enabled\n"
        + "Development runs  enabled\n", output.toString(StandardCharsets.UTF_8));
  }
}
