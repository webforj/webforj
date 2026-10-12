package com.webforj.bbjship.cli.command;

import static com.webforj.plugin.foundation.bbj.model.BBjCredentials.FACTORY_ACCOUNT_WARNING;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assumptions.assumeTrue;
import static org.mockito.Mockito.verify;

import com.basis.api.admin.BBjAdminException;
import com.webforj.bbjship.cli.BBjShipCli;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.Map;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import picocli.CommandLine.Help.Ansi;

class DevelopmentEnableCommandTest {

  @TempDir
  Path temporary;

  @Test
  void shouldWriteTheSwitchFileForTheOwnerAndConfirm() throws Exception {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    assumeTrue(home.getFileSystem().supportedFileAttributeViews().contains("posix"),
        "The file system has no owner permissions");
    BBjServices services = new BBjServices();
    services.addBBjShipConfiguration(home);
    ByteArrayOutputStream output = new ByteArrayOutputStream();
    PrintStream stream = new PrintStream(output, true, StandardCharsets.UTF_8);
    BBjShipCli cli = BBjShipCli.create(home).setInput(InputStream.nullInputStream())
        .setEnvironment(Map.of()).setOutput(stream).setError(stream).setAnsi(Ansi.OFF)
        .setConnector((username, password) -> services.getAdmin()).build();

    int status = cli.run("development", "enable");

    Path enabled = home.resolve("plugins/.webforj-bbjship-deployments/development.enabled");
    assertEquals(0, status);
    assertEquals(FACTORY_ACCOUNT_WARNING + "\n" + "Enabled development runs\n",
        output.toString(StandardCharsets.UTF_8));
    assertEquals("development runs are enabled on this machine", Files.readString(enabled));
    assertEquals(PosixFilePermissions.fromString("rw-------"),
        Files.getPosixFilePermissions(enabled));
    verify(services.getAdmin()).release();
  }

  @Test
  void shouldFailWithoutTheSwitchFileWhenBbjServicesRefusesTheCredentials() throws Exception {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    ByteArrayOutputStream output = new ByteArrayOutputStream();
    PrintStream stream = new PrintStream(output, true, StandardCharsets.UTF_8);
    BBjShipCli cli = BBjShipCli.create(home).setInput(InputStream.nullInputStream())
        .setEnvironment(Map.of()).setOutput(stream).setError(stream).setAnsi(Ansi.OFF)
        .setConnector((username, password) -> {
          throw new BBjAdminException("Server failed login: Authentication failed for user"
              + " 'admin'. Invalid username/password.");
        }).build();

    int status = cli.run("development", "enable");

    assertEquals(1, status);
    assertEquals(FACTORY_ACCOUNT_WARNING + "\n"
        + "Command failed. Server failed login: Authentication failed for user 'admin'."
        + " Invalid username/password.\n", output.toString(StandardCharsets.UTF_8));
    assertFalse(
        Files.exists(home.resolve("plugins/.webforj-bbjship-deployments/development.enabled")));
  }
}
