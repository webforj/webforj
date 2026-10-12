package com.webforj.bbjship.cli.command;

import static com.webforj.plugin.foundation.bbj.model.BBjCredentials.FACTORY_ACCOUNT_WARNING;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.api.admin.BBjAdminAppDeploymentApplication;
import com.basis.api.admin.BBjAdminAppDeploymentConfiguration;
import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.rest.adapter.BBjAdminAdapterList;
import com.webforj.bbjship.cli.BBjShipCli;
import com.webforj.plugin.foundation.bbj.DeploymentArchive;
import com.webforj.plugin.foundation.bbj.HelloApp;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import picocli.CommandLine.Help.Ansi;

class AppsUndeployCommandTest {

  @TempDir
  Path temporary;

  private Path deployments;
  private ByteArrayOutputStream output;
  private BBjAdminBase admin;
  private BBjAdminAppDeploymentConfiguration configuration;
  private BBjShipCli cli;

  @BeforeEach
  void initBBjShipWithHelloAppAndShopDeployed() throws Exception {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Path archive = HelloApp.writeArchive(HelloApp.createCopy(temporary.resolve("hello-app")),
        temporary.resolve("hello-app.zip"));
    deployments = home.resolve("plugins/.webforj-bbjship-deployments/deployments");
    BBjServices services = new BBjServices();
    services.addBBjShipConfiguration(home);
    admin = services.getAdmin();
    configuration = mock(BBjAdminAppDeploymentConfiguration.class);
    when(admin.getRemoteConfiguration()).thenReturn(configuration);
    when(admin.getBBjProcesses()).thenReturn(List.of());
    BBjAdminAdapterList<BBjAdminAppDeploymentApplication> registrations =
        new BBjAdminAdapterList<>();
    for (String name : List.of("hello-app", "shop")) {
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
      services.addClasspath("webforj_bbjship_" + name, List.of(directory.toString()));
      registrations.add(registration);
    }

    when(configuration.getApplications()).thenReturn(registrations);
    output = new ByteArrayOutputStream();
    PrintStream stream = new PrintStream(output, true, StandardCharsets.UTF_8);
    cli = BBjShipCli.create(home).setInput(InputStream.nullInputStream()).setEnvironment(Map.of())
        .setOutput(stream).setError(stream).setAnsi(Ansi.OFF)
        .setConnector((username, password) -> admin).build();
  }

  @Test
  void shouldUndeployTheNamedApplication() throws Exception {
    int status = cli.run("apps", "undeploy", "hello-app");

    assertEquals(0, status);
    assertEquals(FACTORY_ACCOUNT_WARNING + "\n" + "Undeployed hello-app\n",
        output.toString(StandardCharsets.UTF_8));
    verify(configuration).removeApplication("hello-app");
    assertFalse(Files.exists(deployments.resolve("hello-app")));
    assertTrue(Files.isDirectory(deployments.resolve("shop")));
  }

  @Test
  void shouldUndeployEveryApplication() {
    int status = cli.run("apps", "undeploy", "--all");

    assertEquals(0, status);
    assertEquals(FACTORY_ACCOUNT_WARNING + "\n" + """
        Undeployed hello-app
        Undeployed shop
        """, output.toString(StandardCharsets.UTF_8));
    assertFalse(Files.exists(deployments.resolve("hello-app")));
    assertFalse(Files.exists(deployments.resolve("shop")));
  }

  @Test
  void shouldReportAnApplicationTheBbjShipDidNotDeploy() {
    int status = cli.run("apps", "undeploy", "inventory");

    assertEquals(1, status);
    assertEquals(
        FACTORY_ACCOUNT_WARNING + "\n"
            + "Command failed. Application inventory is not deployed by BBjShip\n",
        output.toString(StandardCharsets.UTF_8));
    verify(admin).release();
  }
}
