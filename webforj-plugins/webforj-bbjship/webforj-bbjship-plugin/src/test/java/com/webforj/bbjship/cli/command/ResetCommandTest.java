package com.webforj.bbjship.cli.command;

import static com.webforj.bbjship.cli.command.BBjServices.ASSETS_CONTEXT;
import static com.webforj.bbjship.cli.command.BBjServices.BBJSHIP_CONTEXT;
import static com.webforj.bbjship.cli.command.BBjServices.CLI_CLASSPATH;
import static com.webforj.bbjship.cli.command.Home.ASSETS_FOLDER;
import static com.webforj.bbjship.cli.command.Home.DEPLOYMENTS_FOLDER;
import static com.webforj.bbjship.cli.command.Home.HTDOCS_FOLDER;
import static com.webforj.bbjship.foundation.model.BBjCredentials.FACTORY_ACCOUNT_WARNING;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.api.admin.BBjAdminAppDeploymentApplication;
import com.basis.api.admin.BBjAdminAppDeploymentConfiguration;
import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.rest.adapter.BBjAdminAdapterList;
import com.webforj.bbjship.cli.BBjShipCli;
import com.webforj.bbjship.cli.TerminalLines;
import com.webforj.bbjship.foundation.BBjShipPlugin;
import com.webforj.bbjship.foundation.DeploymentArchive;
import com.webforj.bbjship.foundation.HelloApp;
import com.webforj.bbjship.foundation.ServicesProperties;
import java.io.ByteArrayOutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import picocli.CommandLine.Help.Ansi;

class ResetCommandTest {

  private static final String INVENTORY_APP_NAME = "inventory";
  private static final String PROOF_APP_NAME = "proof-app";
  private static final String BACKED_UP_PROPERTIES = "basis.cacheDirectory=/opt/bbx/cache\n";
  private static final String RESET_COMMAND = "reset";
  private static final String JAVA_ARGUMENTS_LINE = "%s=-Xmx1g%n";
  private static final String YES_OPTION = "--yes";
  private static final String PROGRAM_FILE = "webforj.bbj";

  @TempDir
  Path temporary;

  private Path home;
  private Path deployments;
  private Path developmentSwitch;
  private String settings;
  private Path properties;
  private Path backup;
  private BBjServices bbjServices;
  private BBjAdminBase admin;
  private BBjAdminAppDeploymentConfiguration configuration;
  private ByteArrayOutputStream output;

  @BeforeEach
  void initBBjShipWithInventoryAndProofAppDeployedAndDevelopmentRunsEnabled() throws Exception {
    home = Home.createCopy(temporary.resolve("bbx"));
    Path archive = HelloApp.writeArchive(HelloApp.createCopy(temporary.resolve("hello-app")),
        temporary.resolve("hello-app.zip"));
    deployments =
        Files.createDirectories(home.resolve("plugins/.webforj-bbjship-deployments/deployments"));
    developmentSwitch =
        Files.writeString(home.resolve("plugins/.webforj-bbjship-deployments/development.enabled"),
            "development runs are enabled on this machine");
    settings = """
        %s=/opt/jdk-21/bin/java
        %s=-Xmx1g -XX:VMOptionsFile=/opt/webforj/hotswap.options
        """.formatted(ServicesProperties.JAVA_EXECUTABLE_KEY,
        ServicesProperties.JAVA_ARGUMENTS_KEY);
    properties = Files.writeString(home.resolve("cfg/BBj.properties"), settings);
    backup = home.resolve("plugins/.webforj-bbjship-deployments/BBj.properties.before-webforj");
    bbjServices = new BBjServices();
    bbjServices.addBBjShipConfiguration(home);
    bbjServices.addContext(ASSETS_CONTEXT, """
        {
          "STOP": {"EXECUTOR": "CONTEXT:webforj-bbjship-assets", "DATA": {"ACTION": "STOP"},
            "ENABLED": true},
          "Document Base": "%s"
        }""".formatted(home.resolve(ASSETS_FOLDER)));
    bbjServices.addContext(BBJSHIP_CONTEXT, """
        {
          "STOP": {"EXECUTOR": "CONTEXT:webforj-bbjship", "DATA": {"ACTION": "STOP"},
            "ENABLED": true},
          "HTTP port": 8899,
          "Document Base": "%s"
        }""".formatted(home.resolve(HTDOCS_FOLDER)));
    admin = bbjServices.getAdmin();
    configuration = mock(BBjAdminAppDeploymentConfiguration.class);
    when(admin.getRemoteConfiguration()).thenReturn(configuration);
    when(admin.getBBjProcesses()).thenReturn(List.of());
    BBjAdminAdapterList<BBjAdminAppDeploymentApplication> registrations =
        new BBjAdminAdapterList<>();
    for (String name : List.of(INVENTORY_APP_NAME, PROOF_APP_NAME)) {
      Path directory = Files.createDirectories(deployments.resolve(name))
          .resolve("0b8f4f0e-3f5c-4a8e-9a49-5d1a6f3c2b10");
      DeploymentArchive.read(archive, directory);
      BBjAdminAppDeploymentApplication registration = mock(BBjAdminAppDeploymentApplication.class);
      when(registration.getString(BBjAdminAppDeploymentApplication.NAME)).thenReturn(name);
      when(registration.getString(BBjAdminAppDeploymentApplication.PROGRAM))
          .thenReturn(directory.resolve("bbj/webforj.bbj").toString());
      when(registration.getString(BBjAdminAppDeploymentApplication.CLASSPATH))
          .thenReturn("webforj_bbjship_" + name);
      when(registration.getProperties()).thenReturn(Map.of());
      when(registration.getArguments()).thenReturn(new BBjAdminAdapterList<>());
      when(registration.getJavaArguments()).thenReturn(new BBjAdminAdapterList<>());
      bbjServices.addClasspath("webforj_bbjship_" + name, List.of(directory.toString()));
      registrations.add(registration);
    }

    when(configuration.getApplications()).thenReturn(registrations);
    output = new ByteArrayOutputStream();
  }

  @Test
  void shouldPlanTheResetAndChangeNothingWhenItIsNotConfirmed() throws Exception {
    Files.writeString(backup, BACKED_UP_PROPERTIES);
    BBjShipCli cli = createCli(home, admin, "n\n");

    int status = cli.run(RESET_COMMAND);

    assertEquals(1, status);
    assertEquals(FACTORY_ACCOUNT_WARNING + "\n" + """
        This resets BBjShip on this machine:
          undeploy inventory
          undeploy proof-app
          disable development runs
          remove the BBjShip HTTP endpoint and assets
          restore the Java settings of BBjServices
        Continue? [y/N]
        Reset cancelled
        """, output.toString(StandardCharsets.UTF_8));
    verify(configuration, never()).removeApplication(anyString());
    assertTrue(Files.isDirectory(deployments.resolve(INVENTORY_APP_NAME)));
    assertTrue(Files.isDirectory(deployments.resolve(PROOF_APP_NAME)));
    assertTrue(Files.exists(developmentSwitch));
    assertNotNull(bbjServices.getContext(BBJSHIP_CONTEXT));
    assertNotNull(bbjServices.getContext(ASSETS_CONTEXT));
    assertEquals(List.of(CLI_CLASSPATH, "_webforj_bbjship", "webforj_bbjship_inventory",
        "webforj_bbjship_proof-app"), admin.getClasspathNames());
    assertTrue(Files.exists(backup));
    verify(admin).release();
    verify(admin, never()).shutdownBBjServices(false);
  }

  @Test
  void shouldRefuseToAskForConfirmationOnWindowsBeforeTheLogin() throws Exception {
    Writer stream = new OutputStreamWriter(output, StandardCharsets.UTF_8);
    TerminalLines terminal = new TerminalLines(stream).setTyped("y");
    List<String> opened = new ArrayList<>();
    BBjShipCli cli = BBjShipCli.create(new BBjShipPlugin(home)).setTerminal(terminal)
        .setOperatingSystem("Windows 11").setAnsi(Ansi.OFF).setSessions(login -> {
          opened.add(login.getUsername());
          return admin;
        }).build();

    int status = cli.run(RESET_COMMAND);

    assertEquals(2, status);
    assertEquals(List.of(), opened);
    assertEquals("""
        Standard input is not available on Windows. Run reset --yes to confirm
        """, output.toString(StandardCharsets.UTF_8));
    assertEquals("y", terminal.readLine());
    assertEquals(settings, Files.readString(properties));
    verify(configuration, never()).removeApplication(anyString());
    assertTrue(Files.isDirectory(deployments.resolve(INVENTORY_APP_NAME)));
    assertTrue(Files.isDirectory(deployments.resolve(PROOF_APP_NAME)));
    assertTrue(Files.exists(developmentSwitch));
    assertNotNull(bbjServices.getContext(BBJSHIP_CONTEXT));
    assertNotNull(bbjServices.getContext(ASSETS_CONTEXT));
    assertEquals(List.of(CLI_CLASSPATH, "_webforj_bbjship", "webforj_bbjship_inventory",
        "webforj_bbjship_proof-app"), admin.getClasspathNames());
    verify(admin, never()).release();
  }

  @Test
  void shouldResetAfterTheConfirmationTypedAfterThePassword() throws Exception {
    settings = JAVA_ARGUMENTS_LINE.formatted(ServicesProperties.JAVA_ARGUMENTS_KEY);
    Files.writeString(properties, settings);
    BBjShipCli cli = createCli(home, admin, "secret\ny\n");

    int status = cli.run(RESET_COMMAND, "--username", "administrator", "--password-stdin");

    assertEquals(0, status);
    assertEquals("""
        Reading the password from standard input
        This resets BBjShip on this machine:
          undeploy inventory
          undeploy proof-app
          disable development runs
          remove the BBjShip HTTP endpoint and assets
        Continue? [y/N]
        Undeployed inventory
        Undeployed proof-app
        Disabled development runs
        Disabled the BBjShip HTTP endpoint
        Removed the BBjShip assets
        Reset BBjShip
        """, output.toString(StandardCharsets.UTF_8));
    assertFalse(Files.exists(home.resolve(DEPLOYMENTS_FOLDER)));
    verify(admin, never()).shutdownBBjServices(false);
  }

  @Test
  void shouldResetWithoutAskingWithYesAndLeaveBbjServicesRunning() throws Exception {
    Files.writeString(backup, BACKED_UP_PROPERTIES);
    BBjShipCli cli = createCli(home, admin, "");

    int status = cli.run(RESET_COMMAND, YES_OPTION);

    assertEquals(0, status);
    assertEquals(FACTORY_ACCOUNT_WARNING + "\n" + """
        This resets BBjShip on this machine:
          undeploy inventory
          undeploy proof-app
          disable development runs
          remove the BBjShip HTTP endpoint and assets
          restore the Java settings of BBjServices
        Undeployed inventory
        Undeployed proof-app
        Disabled development runs
        Disabled the BBjShip HTTP endpoint
        Removed the BBjShip assets
        Restored the Java settings of BBjServices
        Reset BBjShip
        Restart BBjServices now to apply the restored Java settings
        """, output.toString(StandardCharsets.UTF_8));
    verify(configuration).removeApplication(INVENTORY_APP_NAME);
    verify(configuration).removeApplication(PROOF_APP_NAME);
    assertFalse(Files.exists(deployments.resolve(INVENTORY_APP_NAME)));
    assertFalse(Files.exists(deployments.resolve(PROOF_APP_NAME)));
    assertFalse(Files.exists(developmentSwitch));
    assertNull(bbjServices.getContext(BBJSHIP_CONTEXT));
    assertNull(bbjServices.getContext(ASSETS_CONTEXT));
    assertEquals(List.of(), admin.getClasspathNames());
    assertEquals(BACKED_UP_PROPERTIES, Files.readString(properties));
    assertFalse(Files.exists(backup));
    assertFalse(Files.exists(home.resolve(DEPLOYMENTS_FOLDER)));
    verify(admin).release();
    verify(admin, never()).shutdownBBjServices(false);
  }

  @Test
  void shouldRestoreThePropertiesFileGivenWithItsBackup() throws Exception {
    Path elsewhere = Files.createDirectories(temporary.resolve("services"));
    Path otherProperties = Files.writeString(elsewhere.resolve("BBj.properties"), settings);
    Path otherBackup = Files.writeString(
        home.resolve("plugins/.webforj-bbjship-deployments/BBj.properties.before-webforj"),
        BACKED_UP_PROPERTIES);
    BBjShipCli cli = createCli(home, admin, "");

    int status = cli.run(RESET_COMMAND, YES_OPTION, "--properties", otherProperties.toString());

    assertEquals(0, status);
    assertEquals(BACKED_UP_PROPERTIES, Files.readString(otherProperties));
    assertFalse(Files.exists(otherBackup));
    assertEquals(settings, Files.readString(properties));
    verify(admin, never()).shutdownBBjServices(false);
  }

  @Test
  void shouldRemoveTheHotswapArgumentWithoutBackupAndLeaveBbjServicesRunning() throws Exception {
    BBjShipCli cli = createCli(home, admin, "");

    int status = cli.run(RESET_COMMAND, YES_OPTION);

    assertEquals(0, status);
    assertEquals(FACTORY_ACCOUNT_WARNING + "\n" + """
        This resets BBjShip on this machine:
          undeploy inventory
          undeploy proof-app
          disable development runs
          remove the BBjShip HTTP endpoint and assets
          restore the Java settings of BBjServices
        Undeployed inventory
        Undeployed proof-app
        Disabled development runs
        Disabled the BBjShip HTTP endpoint
        Removed the BBjShip assets
        Restored the Java settings of BBjServices
        Reset BBjShip
        Restart BBjServices now to apply the restored Java settings
        """, output.toString(StandardCharsets.UTF_8));
    ServicesProperties reset = ServicesProperties.load(properties, temporary);
    assertEquals(List.of("-Xmx1g"), reset.getJavaArguments());
    assertEquals(Optional.of(Path.of("/opt/jdk-21/bin/java")), reset.getJavaExecutable());
    assertFalse(Files.exists(backup));
    verify(admin).release();
    verify(admin, never()).shutdownBBjServices(false);
  }

  @Test
  void shouldReleaseTheSessionWithoutRestartWhenNoBackupExists() throws Exception {
    settings = JAVA_ARGUMENTS_LINE.formatted(ServicesProperties.JAVA_ARGUMENTS_KEY);
    Files.writeString(properties, settings);
    BBjShipCli cli = createCli(home, admin, "");

    int status = cli.run(RESET_COMMAND, YES_OPTION);

    assertEquals(0, status);
    assertEquals(FACTORY_ACCOUNT_WARNING + "\n" + """
        This resets BBjShip on this machine:
          undeploy inventory
          undeploy proof-app
          disable development runs
          remove the BBjShip HTTP endpoint and assets
        Undeployed inventory
        Undeployed proof-app
        Disabled development runs
        Disabled the BBjShip HTTP endpoint
        Removed the BBjShip assets
        Reset BBjShip
        """, output.toString(StandardCharsets.UTF_8));
    assertEquals(settings, Files.readString(properties));
    assertFalse(Files.exists(home.resolve(DEPLOYMENTS_FOLDER)));
    verify(admin).release();
    verify(admin, never()).shutdownBBjServices(false);
  }

  @Test
  void shouldReportTheRuntimeFolderWhoseFilesCannotBeRemovedAndEndAsDone() throws Exception {
    assumeTrue(deployments.getFileSystem().supportedFileAttributeViews().contains("posix"),
        "The removal failure needs a posix file system");
    Files.writeString(properties,
        JAVA_ARGUMENTS_LINE.formatted(ServicesProperties.JAVA_ARGUMENTS_KEY));
    Path held = deployments.resolve("inventory/0b8f4f0e-3f5c-4a8e-9a49-5d1a6f3c2b10/bbj");
    assertTrue(held.toFile().setWritable(false));
    BBjShipCli cli = createCli(home, admin, "");

    int status;
    try {
      status = cli.run(RESET_COMMAND, YES_OPTION);
    } finally {
      assertTrue(held.toFile().setWritable(true));
    }

    assertEquals(0, status);
    assertEquals(
        FACTORY_ACCOUNT_WARNING + "\n"
            + """
                This resets BBjShip on this machine:
                  undeploy inventory
                  undeploy proof-app
                  disable development runs
                  remove the BBjShip HTTP endpoint and assets
                Undeployed inventory
                Not removed: %s. %s
                Undeployed proof-app
                Disabled development runs
                Disabled the BBjShip HTTP endpoint
                Removed the BBjShip assets
                Not removed: %s. %s
                Reset BBjShip
                """.formatted(deployments.resolve(INVENTORY_APP_NAME), held.resolve(PROGRAM_FILE),
                home.resolve(DEPLOYMENTS_FOLDER), held.resolve(PROGRAM_FILE)),
        output.toString(StandardCharsets.UTF_8));
    verify(configuration).removeApplication(INVENTORY_APP_NAME);
    verify(configuration).removeApplication(PROOF_APP_NAME);
    assertEquals(List.of(), admin.getClasspathNames());
    assertNull(bbjServices.getContext(BBJSHIP_CONTEXT));
    assertTrue(Files.isRegularFile(held.resolve(PROGRAM_FILE)));
    assertFalse(Files.exists(deployments.resolve(PROOF_APP_NAME)));
    verify(admin).release();
    verify(admin, never()).shutdownBBjServices(false);
  }

  @Test
  void shouldReportNothingToResetAndRemoveWhatTheSessionAdded() throws Exception {
    Path emptyHome = Home.createCopy(temporary.resolve("empty-bbx"));
    BBjServices emptyServices = new BBjServices();
    BBjShipCli cli = createCli(emptyHome, emptyServices.getAdmin(), "");

    int status = cli.run(RESET_COMMAND);

    assertEquals(0, status);
    assertEquals(FACTORY_ACCOUNT_WARNING + "\n" + "Nothing to reset\n",
        output.toString(StandardCharsets.UTF_8));
    assertNull(emptyServices.getContext(ASSETS_CONTEXT));
    assertEquals(List.of(), emptyServices.getAdmin().getClasspathNames());
    assertFalse(Files.exists(emptyHome.resolve(DEPLOYMENTS_FOLDER)));
    verify(emptyServices.getAdmin()).release();
    verify(emptyServices.getAdmin(), never()).shutdownBBjServices(false);
  }

  @Test
  void shouldRemoveTheUnencryptedSwitchWithTheRuntimeFolder() throws Exception {
    Files.writeString(backup, BACKED_UP_PROPERTIES);
    Path unencryptedSwitch =
        Files.writeString(home.resolve("plugins/.webforj-bbjship-deployments/unencrypted.enabled"),
            "unencrypted requests from another machine are allowed on this machine");
    BBjShipCli cli = createCli(home, admin, "");

    int status = cli.run(RESET_COMMAND, YES_OPTION);

    assertEquals(0, status);
    assertEquals(FACTORY_ACCOUNT_WARNING + "\n" + """
        This resets BBjShip on this machine:
          undeploy inventory
          undeploy proof-app
          disable development runs
          refuse unencrypted requests from another machine
          remove the BBjShip HTTP endpoint and assets
          restore the Java settings of BBjServices
        Undeployed inventory
        Undeployed proof-app
        Disabled development runs
        Refused unencrypted requests from another machine
        Disabled the BBjShip HTTP endpoint
        Removed the BBjShip assets
        Restored the Java settings of BBjServices
        Reset BBjShip
        Restart BBjServices now to apply the restored Java settings
        """, output.toString(StandardCharsets.UTF_8));
    assertFalse(Files.exists(unencryptedSwitch));
    assertFalse(Files.exists(home.resolve(DEPLOYMENTS_FOLDER)));
  }

  @Test
  void shouldPlanTheRemovalOfTheRuntimeFolderThatHoldsPreservedInstallation() throws Exception {
    Path emptyHome = Home.createCopy(temporary.resolve("empty-bbx"));
    Path preserved = Files.createDirectories(emptyHome
        .resolve("plugins/.webforj-bbjship-deployments/.webforj-2041/previous/webforj-bbjship"));
    BBjServices emptyServices = new BBjServices();
    BBjShipCli cli = createCli(emptyHome, emptyServices.getAdmin(), "");

    int status = cli.run(RESET_COMMAND, YES_OPTION);

    assertEquals(0, status);
    assertEquals(FACTORY_ACCOUNT_WARNING + "\n" + """
        This resets BBjShip on this machine:
          remove the BBjShip runtime folder
        Reset BBjShip
        """, output.toString(StandardCharsets.UTF_8));
    assertFalse(Files.exists(preserved));
    assertFalse(Files.exists(emptyHome.resolve(DEPLOYMENTS_FOLDER)));
    verify(emptyServices.getAdmin()).release();
  }

  private BBjShipCli createCli(Path bbjHome, BBjAdminBase bbjAdmin, String input) {
    Writer stream = new OutputStreamWriter(output, StandardCharsets.UTF_8);

    return BBjShipCli.create(new BBjShipPlugin(bbjHome))
        .setTerminal(new TerminalLines(stream).setTyped(input.lines().toArray(String[]::new)))
        .setAnsi(Ansi.OFF).setSessions(login -> bbjAdmin).build();
  }
}
