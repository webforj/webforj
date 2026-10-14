package com.webforj.bbjship.cli.command;

import static com.webforj.bbjship.foundation.model.BBjCredentials.FACTORY_ACCOUNT_WARNING;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;
import static org.mockito.Mockito.mock;
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
import java.io.ByteArrayOutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
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

  private static final String HELLO_APP_NAME = "hello-app";
  private static final String UNDEPLOY_COMMAND = "undeploy";

  @TempDir
  Path temporary;

  private Path deployments;
  private BBjServices services;
  private ByteArrayOutputStream output;
  private BBjAdminBase admin;
  private BBjAdminAppDeploymentConfiguration configuration;
  private BBjShipCli cli;

  @BeforeEach
  void initBBjShipWithHelloAppAndShopDeployed() throws Exception {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Path archive = HelloApp.writeArchive(HelloApp.createCopy(temporary.resolve(HELLO_APP_NAME)),
        temporary.resolve("hello-app.zip"));
    deployments = home.resolve("plugins/.webforj-bbjship-deployments/deployments");
    services = new BBjServices();
    services.addBBjShipConfiguration(home);
    admin = services.getAdmin();
    configuration = mock(BBjAdminAppDeploymentConfiguration.class);
    when(admin.getRemoteConfiguration()).thenReturn(configuration);
    when(admin.getBBjProcesses()).thenReturn(List.of());
    BBjAdminAdapterList<BBjAdminAppDeploymentApplication> registrations =
        new BBjAdminAdapterList<>();
    for (String name : List.of(HELLO_APP_NAME, "shop")) {
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
    Writer stream = new OutputStreamWriter(output, StandardCharsets.UTF_8);
    cli = BBjShipCli.create(new BBjShipPlugin(home)).setTerminal(new TerminalLines(stream))
        .setAnsi(Ansi.OFF).setSessions(login -> admin).build();
  }

  @Test
  void shouldUndeployTheNamedApplication() throws Exception {
    int status = cli.run("apps", UNDEPLOY_COMMAND, HELLO_APP_NAME);

    assertEquals(0, status);
    assertEquals(FACTORY_ACCOUNT_WARNING + "\n" + "Undeployed hello-app\n",
        output.toString(StandardCharsets.UTF_8));
    verify(configuration).removeApplication(HELLO_APP_NAME);
    assertFalse(Files.exists(deployments.resolve(HELLO_APP_NAME)));
    assertTrue(Files.isDirectory(deployments.resolve("shop")));
  }

  @Test
  void shouldUndeployEveryApplication() {
    int status = cli.run("apps", UNDEPLOY_COMMAND, "--all");

    assertEquals(0, status);
    assertEquals(FACTORY_ACCOUNT_WARNING + "\n" + """
        Undeployed hello-app
        Undeployed shop
        """, output.toString(StandardCharsets.UTF_8));
    assertFalse(Files.exists(deployments.resolve(HELLO_APP_NAME)));
    assertFalse(Files.exists(deployments.resolve("shop")));
  }

  @Test
  void shouldUndeployWhenTheBbjShipContextBelongsToAnotherApplication() throws Exception {
    services.addContext("webforj-bbjship", """
        {
          "STOP": {"EXECUTOR": "CONTEXT:webforj-bbjship", "DATA": {"ACTION": "STOP"},
            "ENABLED": true},
          "HTTP port": 8899,
          "Document Base": "%s"
        }""".formatted(temporary.resolve("other-application/htdocs")));

    int status = cli.run("apps", UNDEPLOY_COMMAND, HELLO_APP_NAME);

    assertEquals(0, status);
    assertEquals(FACTORY_ACCOUNT_WARNING + "\n" + "Undeployed hello-app\n",
        output.toString(StandardCharsets.UTF_8));
    verify(configuration).removeApplication(HELLO_APP_NAME);
    assertFalse(Files.exists(deployments.resolve(HELLO_APP_NAME)));
  }

  @Test
  void shouldReportTheFolderWhoseFilesCannotBeRemoved() throws Exception {
    assumeTrue(deployments.getFileSystem().supportedFileAttributeViews().contains("posix"),
        "The removal failure needs a posix file system");
    Path held = deployments.resolve("hello-app/0b8f4f0e-3f5c-4a8e-9a49-5d1a6f3c2b10/bbj");
    assertTrue(held.toFile().setWritable(false));

    int status;
    try {
      status = cli.run("apps", UNDEPLOY_COMMAND, "--all");
    } finally {
      assertTrue(held.toFile().setWritable(true));
    }

    assertEquals(0, status);
    assertEquals(FACTORY_ACCOUNT_WARNING + "\n" + """
        Undeployed hello-app
        Not removed: %s. %s
        Undeployed shop
        """.formatted(deployments.resolve(HELLO_APP_NAME), held.resolve("webforj.bbj")),
        output.toString(StandardCharsets.UTF_8));
    verify(configuration).removeApplication(HELLO_APP_NAME);
    assertTrue(Files.isRegularFile(held.resolve("webforj.bbj")));
    assertFalse(Files.exists(deployments.resolve("shop")));
  }

  @Test
  void shouldReportAnApplicationTheBbjShipDidNotDeploy() {
    int status = cli.run("apps", UNDEPLOY_COMMAND, "inventory");

    assertEquals(1, status);
    assertEquals(
        FACTORY_ACCOUNT_WARNING + "\n"
            + "Command failed. Application inventory is not deployed by BBjShip\n",
        output.toString(StandardCharsets.UTF_8));
    verify(admin).release();
  }
}
