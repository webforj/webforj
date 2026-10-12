package com.webforj.bbjship.cli.command;

import static com.webforj.plugin.foundation.bbj.model.BBjCredentials.FACTORY_ACCOUNT_WARNING;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.verify;

import com.basis.api.admin.BBjAdminException;
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
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import picocli.CommandLine.Help.Ansi;

class DevelopmentDisableCommandTest {

  @TempDir
  Path temporary;

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldRemoveTheSwitchFileAndConfirm(boolean enabled) throws Exception {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Path file = home.resolve("plugins/.webforj-bbjship-deployments/development.enabled");
    if (enabled) {
      Files.createDirectories(file.getParent());
      Files.writeString(file, "development runs are enabled on this machine");
    }

    BBjServices services = new BBjServices();
    services.addBBjShipConfiguration(home);
    ByteArrayOutputStream output = new ByteArrayOutputStream();
    PrintStream stream = new PrintStream(output, true, StandardCharsets.UTF_8);
    BBjShipCli cli = BBjShipCli.create(home).setInput(InputStream.nullInputStream())
        .setEnvironment(Map.of()).setOutput(stream).setError(stream).setAnsi(Ansi.OFF)
        .setConnector((username, password) -> services.getAdmin()).build();

    int status = cli.run("development", "disable");

    assertEquals(0, status);
    assertEquals(FACTORY_ACCOUNT_WARNING + "\n" + "Disabled development runs\n",
        output.toString(StandardCharsets.UTF_8));
    assertFalse(Files.exists(file));
    verify(services.getAdmin()).release();
  }

  @Test
  void shouldKeepTheSwitchFileWhenBbjServicesRefusesTheCredentials() throws Exception {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Path runtime = Files.createDirectories(home.resolve("plugins/.webforj-bbjship-deployments"));
    Path file = Files.writeString(runtime.resolve("development.enabled"),
        "development runs are enabled on this machine");
    ByteArrayOutputStream output = new ByteArrayOutputStream();
    PrintStream stream = new PrintStream(output, true, StandardCharsets.UTF_8);
    BBjShipCli cli = BBjShipCli.create(home).setInput(InputStream.nullInputStream())
        .setEnvironment(Map.of()).setOutput(stream).setError(stream).setAnsi(Ansi.OFF)
        .setConnector((username, password) -> {
          throw new BBjAdminException("Server failed login: Authentication failed for user"
              + " 'admin'. Invalid username/password.");
        }).build();

    int status = cli.run("development", "disable");

    assertEquals(1, status);
    assertEquals(FACTORY_ACCOUNT_WARNING + "\n"
        + "Command failed. Server failed login: Authentication failed for user 'admin'."
        + " Invalid username/password.\n", output.toString(StandardCharsets.UTF_8));
    assertTrue(Files.exists(file));
  }
}
