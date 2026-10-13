package com.webforj.plugin.foundation.bbj.setup;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import com.webforj.plugin.foundation.bbj.BBjShipPackage;
import com.webforj.plugin.foundation.bbj.Home;
import com.webforj.plugin.foundation.bbj.Setup;
import com.webforj.plugin.foundation.bbj.model.BBjCredentials;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.Answers;
import org.mockito.InOrder;
import org.mockito.MockedConstruction;

class SetupConfigurationTest {

  private static final String PLUGIN_NAME = "webforj-bbjship";
  private static final String PLUGIN_ZIP = "webforj-bbjship.zip";
  private static final String SHIP_VERSION = "26.03";
  private static final String DEPLOYMENT_URL = "http://localhost:1/webforj-bbjship";
  private static final String ADMIN_USERNAME = "administrator";
  private static final String ADMIN_PASSWORD = "secret-password";
  private static final String VERSION_FILE = "version.properties";
  private static final String PREVIOUS_VERSION_LINE = "version=26.02\n";
  private static final String PLUGIN_PATH = "plugins/webforj-bbjship";

  @TempDir
  Path temporary;

  @ParameterizedTest
  @ValueSource(booleans = {false, true})
  void shouldKeepBbjShipFilesWhenTheTimedOutCommandDoesNotStop(boolean interrupted)
      throws Exception {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Path plugin = BBjShipPackage.createCopy(temporary.resolve(PLUGIN_NAME));
    Path zip = BBjShipPackage.writeZip(plugin, temporary.resolve(PLUGIN_ZIP));
    List<String> messages = new ArrayList<>();
    List<String> warnings = new ArrayList<>();
    Setup setup =
        Setup.create(zip, SHIP_VERSION).setHome(home).setDeploymentUrl(URI.create(DEPLOYMENT_URL))
            .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD)).setPort(8888)
            .setLog(messages::add).setWarn(warnings::add).build();
    Process command = mock(Process.class);
    when(command.getOutputStream()).thenReturn(new ByteArrayOutputStream());
    when(command.waitFor(120, TimeUnit.SECONDS)).thenReturn(false);
    when(command.isAlive()).thenReturn(true);
    if (interrupted) {
      when(command.waitFor(10, TimeUnit.SECONDS)).thenThrow(new InterruptedException());
    } else {
      when(command.waitFor(10, TimeUnit.SECONDS)).thenReturn(false);
    }

    try (MockedConstruction<ProcessBuilder> builders =
        mockConstruction(ProcessBuilder.class, withSettings().defaultAnswer(Answers.RETURNS_SELF),
            (builder, context) -> when(builder.start()).thenReturn(command))) {
      IOException failure = assertThrows(IOException.class, setup::apply);

      assertEquals(
          "BBj setup command did not stop. Resolve the running setup command before retrying",
          failure.getMessage());
      assertEquals(1, builders.constructed().size());
      assertEquals(interrupted, Thread.currentThread().isInterrupted());
      InOrder calls = inOrder(command);
      calls.verify(command).destroyForcibly();
      calls.verify(command).waitFor(10, TimeUnit.SECONDS);
    } finally {
      Thread.interrupted();
    }

    assertEquals("version=26.03\n",
        Files.readString(home.resolve("plugins/webforj-bbjship/version.properties")));
    assertEquals(List.of("BBj setup command is still running. Plugin files have been preserved"),
        warnings);
    assertFalse(messages.contains("BBj setup completed"));
  }

  @Test
  @SuppressWarnings("try")
  void shouldPreserveThePreviousBbjShipInTheRuntimeFolderWhenTheTimedOutCommandDoesNotStop()
      throws Exception {
    Path home = Home.createCopy(temporary.resolve("bbx")).toRealPath();
    Path previous = BBjShipPackage.createCopy(temporary.resolve("webforj-bbjship-26.02"));
    Files.writeString(previous.resolve(VERSION_FILE), PREVIOUS_VERSION_LINE);
    Setup.create(BBjShipPackage.writeZip(previous, temporary.resolve("previous.zip")), "26.02")
        .setHome(home).setDeploymentUrl(URI.create(DEPLOYMENT_URL))
        .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD)).setPort(8888)
        .build().apply();
    Path plugin = BBjShipPackage.createCopy(temporary.resolve(PLUGIN_NAME));
    Path zip = BBjShipPackage.writeZip(plugin, temporary.resolve(PLUGIN_ZIP));
    List<String> warnings = new ArrayList<>();
    Setup setup =
        Setup.create(zip, SHIP_VERSION).setHome(home).setDeploymentUrl(URI.create(DEPLOYMENT_URL))
            .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD)).setPort(8888)
            .setForce(true).setWarn(warnings::add).build();
    Process command = mock(Process.class);
    when(command.getOutputStream()).thenReturn(new ByteArrayOutputStream());
    when(command.waitFor(120, TimeUnit.SECONDS)).thenReturn(false);
    when(command.isAlive()).thenReturn(true);
    when(command.waitFor(10, TimeUnit.SECONDS)).thenReturn(false);

    try (MockedConstruction<ProcessBuilder> builders =
        mockConstruction(ProcessBuilder.class, withSettings().defaultAnswer(Answers.RETURNS_SELF),
            (builder, context) -> when(builder.start()).thenReturn(command))) {
      assertThrows(IOException.class, setup::apply);
    }

    Path runtime = home.resolve("plugins/.webforj-bbjship-deployments");
    List<Path> transactions;
    try (Stream<Path> folders = Files.list(runtime)) {
      transactions = folders.toList();
    }

    Path backup = transactions.get(0).resolve("previous");
    assertEquals(1, transactions.size());
    assertEquals(PREVIOUS_VERSION_LINE, Files.readString(backup.resolve(VERSION_FILE)));
    assertEquals(List.of("BBj setup command is still running. Plugin files have been preserved",
        "Previous BBjShip files are preserved at " + backup), warnings);
    assertEquals(Set.of(".webforj-bbjship-deployments", PLUGIN_NAME),
        Set.of(home.resolve("plugins").toFile().list()));
  }

  @Test
  void shouldRemoveFreshBbjShipOnlyAfterTheTimedOutCommandStopped() throws Exception {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Path plugin = BBjShipPackage.createCopy(temporary.resolve(PLUGIN_NAME));
    Path zip = BBjShipPackage.writeZip(plugin, temporary.resolve(PLUGIN_ZIP));
    Setup setup =
        Setup.create(zip, SHIP_VERSION).setHome(home).setDeploymentUrl(URI.create(DEPLOYMENT_URL))
            .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD)).setPort(8888)
            .build();
    Process command = mock(Process.class);
    when(command.getOutputStream()).thenReturn(new ByteArrayOutputStream());
    when(command.waitFor(120, TimeUnit.SECONDS)).thenReturn(false);
    when(command.isAlive()).thenReturn(true);
    when(command.waitFor(10, TimeUnit.SECONDS)).thenAnswer(
        invocation -> Files.isRegularFile(home.resolve("plugins/webforj-bbjship/bbjship")));

    try (MockedConstruction<ProcessBuilder> builders =
        mockConstruction(ProcessBuilder.class, withSettings().defaultAnswer(Answers.RETURNS_SELF),
            (builder, context) -> when(builder.start()).thenReturn(command))) {
      IOException failure = assertThrows(IOException.class, setup::apply);

      assertEquals("BBj setup command timed out. Check local BBjServices", failure.getMessage());
      assertEquals(1, builders.constructed().size());
      InOrder calls = inOrder(command);
      calls.verify(command).destroyForcibly();
      calls.verify(command).waitFor(10, TimeUnit.SECONDS);
    }

    assertFalse(Files.exists(home.resolve(PLUGIN_PATH)));
  }

  @Test
  void shouldKeepTheInterruptionAndRemoveFreshBbjShip() throws Exception {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Path plugin = BBjShipPackage.createCopy(temporary.resolve(PLUGIN_NAME));
    Path zip = BBjShipPackage.writeZip(plugin, temporary.resolve(PLUGIN_ZIP));
    Setup setup =
        Setup.create(zip, SHIP_VERSION).setHome(home).setDeploymentUrl(URI.create(DEPLOYMENT_URL))
            .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD)).setPort(8888)
            .build();
    Process command = mock(Process.class);
    when(command.getOutputStream()).thenReturn(new ByteArrayOutputStream());
    when(command.waitFor(120, TimeUnit.SECONDS)).thenThrow(new InterruptedException());

    try (MockedConstruction<ProcessBuilder> builders =
        mockConstruction(ProcessBuilder.class, withSettings().defaultAnswer(Answers.RETURNS_SELF),
            (builder, context) -> when(builder.start()).thenReturn(command))) {
      IOException failure = assertThrows(IOException.class, setup::apply);

      assertEquals("BBj setup was interrupted", failure.getMessage());
      assertEquals(1, builders.constructed().size());
      assertTrue(Thread.currentThread().isInterrupted());
    } finally {
      Thread.interrupted();
    }

    assertFalse(Files.exists(home.resolve(PLUGIN_PATH)));
  }

  @ParameterizedTest
  @ValueSource(strings = "remote")
  void shouldRestorePreviousBbjShipWhenItsConfigurationFails(String failure) throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Path previous = BBjShipPackage.createCopy(temporary.resolve("webforj-bbjship-26.02"));
    Files.writeString(previous.resolve(VERSION_FILE), PREVIOUS_VERSION_LINE);
    Setup.create(BBjShipPackage.writeZip(previous, temporary.resolve("previous.zip")), "26.02")
        .setHome(home).setDeploymentUrl(URI.create(DEPLOYMENT_URL))
        .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD)).setPort(8888)
        .build().apply();
    Path installed = home.resolve(PLUGIN_PATH);
    Files.createDirectories(installed.resolve("deployments/hello-app"));
    Files.writeString(installed.resolve("deployments/hello-app/application.jar"), "hello-app");
    Files.writeString(home.resolve("failure"), failure);
    Path plugin = BBjShipPackage.createCopy(temporary.resolve(PLUGIN_NAME));
    Path zip = BBjShipPackage.writeZip(plugin, temporary.resolve(PLUGIN_ZIP));
    Setup setup =
        Setup.create(zip, SHIP_VERSION).setHome(home).setDeploymentUrl(URI.create(DEPLOYMENT_URL))
            .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD)).setPort(8888)
            .setForce(true).build();

    IOException exception = assertThrows(IOException.class, setup::apply);

    assertEquals("BBj setup command failed. Check credentials and local BBjServices.",
        exception.getMessage());
    assertEquals(PREVIOUS_VERSION_LINE, Files.readString(installed.resolve(VERSION_FILE)));
    assertEquals("hello-app",
        Files.readString(installed.resolve("deployments/hello-app/application.jar")));
  }

  @Test
  void shouldAddTheCommandOutputToTheFailureMessage() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Path plugin = BBjShipPackage.createCopy(temporary.resolve(PLUGIN_NAME));
    Files.writeString(plugin.resolve("bbjship"), """
        #!/bin/sh
        IFS= read -r password
        echo 'Access denied for administrator'
        exit 2
        """);
    Path zip = BBjShipPackage.writeZip(plugin, temporary.resolve(PLUGIN_ZIP));
    Setup setup =
        Setup.create(zip, SHIP_VERSION).setHome(home).setDeploymentUrl(URI.create(DEPLOYMENT_URL))
            .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD)).setPort(8888)
            .build();

    IOException failure = assertThrows(IOException.class, setup::apply);

    assertEquals("BBj setup command failed. Check credentials and local BBjServices. "
        + "Access denied for administrator", failure.getMessage());
  }

  @Test
  void shouldConfigureMatchingInstalledBbjShipWithoutReplacingItsFiles() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Path plugin = BBjShipPackage.createCopy(temporary.resolve(PLUGIN_NAME));
    Path zip = BBjShipPackage.writeZip(plugin, temporary.resolve(PLUGIN_ZIP));
    List<String> messages = new ArrayList<>();
    Setup setup =
        Setup.create(zip, SHIP_VERSION).setHome(home).setDeploymentUrl(URI.create(DEPLOYMENT_URL))
            .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD)).setPort(8888)
            .setLog(messages::add).build();
    setup.apply();
    Files.writeString(home.resolve("plugins/webforj-bbjship/bbj-plugin.json"), "{}");
    messages.clear();

    setup.apply();

    assertEquals("{}", Files.readString(home.resolve("plugins/webforj-bbjship/bbj-plugin.json")));
    assertEquals(
        List.of("Using BBj home " + home.toRealPath(), "BBjShip 26.03 is already installed",
            "Enabling BBj remote deployment", "BBj setup completed"),
        messages);
    assertEquals(2,
        Files.readAllLines(home.resolve("invocations")).stream().filter("END"::equals).count());
  }

  @Test
  void shouldKeepConfiguredSettingsAfterRejectingNullSettings() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Path plugin = BBjShipPackage.createCopy(temporary.resolve(PLUGIN_NAME));
    Path zip = BBjShipPackage.writeZip(plugin, temporary.resolve(PLUGIN_ZIP));
    Setup.Builder builder = Setup.create(zip, SHIP_VERSION);
    assertSame(builder, builder.setHome(home));
    assertSame(builder, builder.setDeploymentUrl(URI.create(DEPLOYMENT_URL)));
    assertSame(builder,
        builder.setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD)));
    assertSame(builder, builder.setPort(8888));
    assertSame(builder, builder.setForce(false));
    assertSame(builder, builder.setLog(message -> {
    }));
    assertSame(builder, builder.setWarn(message -> {
    }));

    BBjCredentials credentials = null;
    assertThrows(NullPointerException.class, () -> builder.setCredentials(credentials));
    Consumer<String> log = null;
    assertThrows(NullPointerException.class, () -> builder.setLog(log));
    Consumer<String> warn = null;
    assertThrows(NullPointerException.class, () -> builder.setWarn(warn));
    Path missingHome = null;
    assertThrows(NullPointerException.class, () -> builder.setHome(missingHome));
    URI deploymentUrl = null;
    assertThrows(NullPointerException.class, () -> builder.setDeploymentUrl(deploymentUrl));

    builder.build().apply();
    assertTrue(Files.exists(home.resolve("remote-enabled")));
  }
}
