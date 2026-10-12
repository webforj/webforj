package com.webforj.bbjship.cli.command;

import static com.webforj.plugin.foundation.bbj.model.BBjCredentials.FACTORY_ACCOUNT_WARNING;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.api.admin.BBjAdminAppDeploymentApplication;
import com.basis.api.admin.BBjAdminAppDeploymentConfiguration;
import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.rest.adapter.BBjAdminAdapterList;
import com.webforj.bbjship.cli.BBjShipCli;
import com.webforj.bbjship.cli.admin.BBjShipServices;
import com.webforj.plugin.foundation.bbj.DeploymentArchive;
import com.webforj.plugin.foundation.bbj.HelloApp;
import com.webforj.plugin.foundation.bbj.ServicesProperties;
import com.webforj.plugin.foundation.bbj.model.BBjCredentials;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.OptionalLong;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import picocli.CommandLine.Help.Ansi;

class ResetCommandTest {

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
  private BBjShipServices services;
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
    bbjServices.addContext("webforj-bbjship-assets", """
        {
          "STOP": {"EXECUTOR": "CONTEXT:webforj-bbjship-assets", "DATA": {"ACTION": "STOP"},
            "ENABLED": true},
          "Document Base": "%s"
        }""".formatted(home.resolve("plugins/.webforj-bbjship-deployments/assets")));
    bbjServices.addContext("webforj-bbjship", """
        {
          "STOP": {"EXECUTOR": "CONTEXT:webforj-bbjship", "DATA": {"ACTION": "STOP"},
            "ENABLED": true},
          "HTTP port": 8899,
          "Document Base": "%s"
        }""".formatted(home.resolve("plugins/.webforj-bbjship-deployments/htdocs")));
    admin = bbjServices.getAdmin();
    configuration = mock(BBjAdminAppDeploymentConfiguration.class);
    when(admin.getRemoteConfiguration()).thenReturn(configuration);
    when(admin.getBBjProcesses()).thenReturn(List.of());
    BBjAdminAdapterList<BBjAdminAppDeploymentApplication> registrations =
        new BBjAdminAdapterList<>();
    for (String name : List.of("inventory", "proof-app")) {
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
    services = mock(BBjShipServices.class);
    when(services.readProcessId(eq(admin), argThat(BBjCredentials::isFactoryAccount)))
        .thenReturn(OptionalLong.of(42));
    output = new ByteArrayOutputStream();
  }

  @Test
  void shouldResetEverythingTheBbjShipConfiguredAfterConfirmation() throws Exception {
    Files.writeString(backup, "basis.cacheDirectory=/opt/bbx/cache\n");
    BBjShipCli cli = createCli(home, admin, "y\n");

    int status = cli.run("reset");

    assertEquals(0, status);
    assertEquals(FACTORY_ACCOUNT_WARNING + "\n" + """
        This resets BBjShip on this machine:
          undeploy inventory
          undeploy proof-app
          disable development runs
          remove the BBjShip HTTP endpoint and assets
          restore the Java settings of BBjServices and restart it
        Continue? [y/N]
        Undeployed inventory
        Undeployed proof-app
        Disabled development runs
        Disabled the BBjShip HTTP endpoint
        Removed the BBjShip assets
        Restored the Java settings of BBjServices
        Restarted BBjServices
        Reset BBjShip
        """, output.toString(StandardCharsets.UTF_8));
    verify(configuration).removeApplication("inventory");
    verify(configuration).removeApplication("proof-app");
    assertFalse(Files.exists(deployments.resolve("inventory")));
    assertFalse(Files.exists(deployments.resolve("proof-app")));
    assertFalse(Files.exists(developmentSwitch));
    assertNull(bbjServices.getContext("webforj-bbjship"));
    assertNull(bbjServices.getContext("webforj-bbjship-assets"));
    assertEquals(List.of(), admin.getClasspathNames());
    assertEquals("basis.cacheDirectory=/opt/bbx/cache\n", Files.readString(properties));
    assertFalse(Files.exists(backup));
    assertFalse(Files.exists(home.resolve("plugins/.webforj-bbjship-deployments")));
    verify(services).restart(admin, OptionalLong.of(42));
  }

  @Test
  void shouldReadTheConfirmationAfterThePasswordFromStandardInput() throws Exception {
    Files.writeString(backup, "basis.cacheDirectory=/opt/bbx/cache\n");
    BBjShipCli cli = createCli(home, admin, "admin123\ny\n");

    int status = cli.run("reset", "--username", "admin", "--password-stdin");

    assertEquals(0, status);
    assertEquals("Reading the password from standard input\n" + FACTORY_ACCOUNT_WARNING + "\n" + """
        This resets BBjShip on this machine:
          undeploy inventory
          undeploy proof-app
          disable development runs
          remove the BBjShip HTTP endpoint and assets
          restore the Java settings of BBjServices and restart it
        Continue? [y/N]
        Undeployed inventory
        Undeployed proof-app
        Disabled development runs
        Disabled the BBjShip HTTP endpoint
        Removed the BBjShip assets
        Restored the Java settings of BBjServices
        Restarted BBjServices
        Reset BBjShip
        """, output.toString(StandardCharsets.UTF_8));
    verify(configuration).removeApplication("inventory");
    verify(configuration).removeApplication("proof-app");
    verify(services).restart(admin, OptionalLong.of(42));
  }

  @Test
  void shouldChangeNothingWhenTheResetIsNotConfirmed() throws Exception {
    Files.writeString(backup, "basis.cacheDirectory=/opt/bbx/cache\n");
    BBjShipCli cli = createCli(home, admin, "n\n");

    int status = cli.run("reset");

    assertEquals(1, status);
    assertEquals(FACTORY_ACCOUNT_WARNING + "\n" + """
        This resets BBjShip on this machine:
          undeploy inventory
          undeploy proof-app
          disable development runs
          remove the BBjShip HTTP endpoint and assets
          restore the Java settings of BBjServices and restart it
        Continue? [y/N]
        Reset cancelled
        """, output.toString(StandardCharsets.UTF_8));
    verify(configuration, never()).removeApplication(anyString());
    assertTrue(Files.isDirectory(deployments.resolve("inventory")));
    assertTrue(Files.isDirectory(deployments.resolve("proof-app")));
    assertTrue(Files.exists(developmentSwitch));
    assertNotNull(bbjServices.getContext("webforj-bbjship"));
    assertNotNull(bbjServices.getContext("webforj-bbjship-assets"));
    assertEquals(List.of("_webforj_bbjship_cli", "_webforj_bbjship", "webforj_bbjship_inventory",
        "webforj_bbjship_proof-app"), admin.getClasspathNames());
    assertTrue(Files.exists(backup));
    verify(admin).release();
    verify(services, never()).restart(any(), any());
  }

  @Test
  void shouldResetWithoutAskingWithYes() throws Exception {
    Files.writeString(backup, "basis.cacheDirectory=/opt/bbx/cache\n");
    BBjShipCli cli = createCli(home, admin, "");

    int status = cli.run("reset", "--yes");

    assertEquals(0, status);
    assertEquals(FACTORY_ACCOUNT_WARNING + "\n" + """
        This resets BBjShip on this machine:
          undeploy inventory
          undeploy proof-app
          disable development runs
          remove the BBjShip HTTP endpoint and assets
          restore the Java settings of BBjServices and restart it
        Undeployed inventory
        Undeployed proof-app
        Disabled development runs
        Disabled the BBjShip HTTP endpoint
        Removed the BBjShip assets
        Restored the Java settings of BBjServices
        Restarted BBjServices
        Reset BBjShip
        """, output.toString(StandardCharsets.UTF_8));
    verify(services).restart(admin, OptionalLong.of(42));
  }

  @Test
  void shouldRestoreThePropertiesFileGivenWithItsBackup() throws Exception {
    Path elsewhere = Files.createDirectories(temporary.resolve("services"));
    Path otherProperties = Files.writeString(elsewhere.resolve("BBj.properties"), settings);
    Path otherBackup = Files.writeString(
        home.resolve("plugins/.webforj-bbjship-deployments/BBj.properties.before-webforj"),
        "basis.cacheDirectory=/opt/bbx/cache\n");
    BBjShipCli cli = createCli(home, admin, "");

    int status = cli.run("reset", "--yes", "--properties", otherProperties.toString());

    assertEquals(0, status);
    assertEquals("basis.cacheDirectory=/opt/bbx/cache\n", Files.readString(otherProperties));
    assertFalse(Files.exists(otherBackup));
    assertEquals(settings, Files.readString(properties));
    verify(services).restart(admin, OptionalLong.of(42));
  }

  @Test
  void shouldReleaseTheSessionWithoutRestartWhenNoBackupExists() throws Exception {
    BBjShipCli cli = createCli(home, admin, "y\n");

    int status = cli.run("reset");

    assertEquals(0, status);
    assertEquals(FACTORY_ACCOUNT_WARNING + "\n" + """
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
    assertEquals(settings, Files.readString(properties));
    assertFalse(Files.exists(home.resolve("plugins/.webforj-bbjship-deployments")));
    verify(admin).release();
    verify(services, never()).readProcessId(any(), any());
    verify(services, never()).restart(any(), any());
  }

  @Test
  void shouldReportNothingToResetAndRemoveWhatTheSessionAdded() throws Exception {
    Path emptyHome = Home.createCopy(temporary.resolve("empty-bbx"));
    BBjServices emptyServices = new BBjServices();
    BBjShipCli cli = createCli(emptyHome, emptyServices.getAdmin(), "");

    int status = cli.run("reset");

    assertEquals(0, status);
    assertEquals(FACTORY_ACCOUNT_WARNING + "\n" + "Nothing to reset\n",
        output.toString(StandardCharsets.UTF_8));
    assertNull(emptyServices.getContext("webforj-bbjship-assets"));
    assertEquals(List.of(), emptyServices.getAdmin().getClasspathNames());
    assertFalse(Files.exists(emptyHome.resolve("plugins/.webforj-bbjship-deployments")));
    verify(emptyServices.getAdmin()).release();
    verify(services, never()).restart(any(), any());
  }

  @Test
  void shouldPlanTheRemovalOfTheRuntimeFolderThatHoldsAPreservedInstallation() throws Exception {
    Path emptyHome = Home.createCopy(temporary.resolve("empty-bbx"));
    Path preserved = Files.createDirectories(emptyHome
        .resolve("plugins/.webforj-bbjship-deployments/.webforj-2041/previous/webforj-bbjship"));
    BBjServices emptyServices = new BBjServices();
    BBjShipCli cli = createCli(emptyHome, emptyServices.getAdmin(), "y\n");

    int status = cli.run("reset");

    assertEquals(0, status);
    assertEquals(FACTORY_ACCOUNT_WARNING + "\n" + """
        This resets BBjShip on this machine:
          remove the BBjShip runtime folder
        Continue? [y/N]
        Reset BBjShip
        """, output.toString(StandardCharsets.UTF_8));
    assertFalse(Files.exists(preserved));
    assertFalse(Files.exists(emptyHome.resolve("plugins/.webforj-bbjship-deployments")));
    verify(emptyServices.getAdmin()).release();
  }

  private BBjShipCli createCli(Path bbjHome, BBjAdminBase bbjAdmin, String input) {
    PrintStream stream = new PrintStream(output, true, StandardCharsets.UTF_8);

    return BBjShipCli.create(bbjHome)
        .setInput(new ByteArrayInputStream(input.getBytes(StandardCharsets.UTF_8)))
        .setEnvironment(Map.of()).setOutput(stream).setError(stream).setAnsi(Ansi.OFF)
        .setConnector((username, password) -> bbjAdmin).setServices(services).build();
  }
}
