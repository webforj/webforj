package com.webforj.bbjship.cli.command;

import static com.webforj.plugin.foundation.bbj.model.BBjCredentials.FACTORY_ACCOUNT_WARNING;
import static org.junit.jupiter.api.Assertions.assertEquals;
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
import java.io.File;
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

class AppsListCommandTest {

  @TempDir
  Path temporary;

  private Path home;
  private Path archive;
  private ByteArrayOutputStream output;
  private BBjAdminBase admin;
  private BBjAdminAppDeploymentConfiguration configuration;
  private Path hello;
  private Path shop;
  private BBjShipCli cli;

  @BeforeEach
  void initBBjShipWithoutApplications() throws Exception {
    home = Home.createCopy(temporary.resolve("bbx"));
    archive = HelloApp.writeArchive(HelloApp.createCopy(temporary.resolve("hello-app")),
        temporary.resolve("hello-app.zip"));
    output = new ByteArrayOutputStream();
    BBjServices services = new BBjServices();
    services.addBBjShipConfiguration(home);
    admin = services.getAdmin();
    configuration = mock(BBjAdminAppDeploymentConfiguration.class);
    when(admin.getRemoteConfiguration()).thenReturn(configuration);
    when(configuration.getApplications()).thenReturn(new BBjAdminAdapterList<>(List.of()));
    PrintStream stream = new PrintStream(output, true, StandardCharsets.UTF_8);
    cli = BBjShipCli.create(home).setInput(InputStream.nullInputStream()).setEnvironment(Map.of())
        .setOutput(stream).setError(stream).setAnsi(Ansi.OFF)
        .setConnector((username, password) -> admin).build();
  }

  @Test
  void shouldListTheNameAndUrlOfTheDeployedApplicationsAsTable() throws Exception {
    deployHelloAppAndShop();

    int status = cli.run("apps", "list");

    assertEquals(0, status);
    assertEquals(FACTORY_ACCOUNT_WARNING + "\n" + """
        NAME       URL
        hello-app  /webapp/hello-app
        shop       /webapp/shop
        """, output.toString(StandardCharsets.UTF_8));
    verify(admin).release();
  }

  @Test
  void shouldListTheDeploymentFolderRelativeToThePluginWhenVerbose() throws Exception {
    deployHelloAppAndShop();

    int status = cli.run("apps", "list", "--verbose");

    assertEquals(0, status);
    assertEquals(FACTORY_ACCOUNT_WARNING + "\n" + """
        NAME       URL                DEPLOYMENT                                                  \
        CLASS FOLDER
        hello-app  /webapp/hello-app  deployments/hello-app/0b8f4f0e-3f5c-4a8e-9a49-5d1a6f3c2b10
        shop       /webapp/shop       deployments/shop/6c1d2e3f-4a5b-4c6d-8e7f-90a1b2c3d4e5
        """, output.toString(StandardCharsets.UTF_8));
  }

  @Test
  void shouldListTheClassFolderOfDevelopmentRunWhenVerbose() throws Exception {
    deployHelloAppAndShop();
    Path classFolder = temporary.resolve("hello-app/src/main/resources");
    Files.writeString(hello.resolve("development"), classFolder.toString());

    int status = cli.run("apps", "list", "--verbose");

    assertEquals(0, status);
    assertEquals(FACTORY_ACCOUNT_WARNING + "\n" + """
        NAME       URL                DEPLOYMENT                                                  \
        CLASS FOLDER
        hello-app  /webapp/hello-app  deployments/hello-app/0b8f4f0e-3f5c-4a8e-9a49-5d1a6f3c2b10  %s
        shop       /webapp/shop       deployments/shop/6c1d2e3f-4a5b-4c6d-8e7f-90a1b2c3d4e5
        """.formatted(classFolder), output.toString(StandardCharsets.UTF_8));
  }

  @Test
  void shouldListEveryClassFolderOfDevelopmentRunWhenVerbose() throws Exception {
    deployHelloAppAndShop();
    Path classes = temporary.resolve("hello-app/src/main/java");
    Path resources = temporary.resolve("hello-app/src/main/resources");
    Files.writeString(hello.resolve("development"), """
        %s
        %s
        """.formatted(classes, resources));

    int status = cli.run("apps", "list", "--verbose");

    assertEquals(0, status);
    assertEquals(
        FACTORY_ACCOUNT_WARNING + "\n"
            + """
                NAME       URL                DEPLOYMENT                                                  \
                CLASS FOLDER
                hello-app  /webapp/hello-app  deployments/hello-app/0b8f4f0e-3f5c-4a8e-9a49-5d1a6f3c2b10  %s
                shop       /webapp/shop       deployments/shop/6c1d2e3f-4a5b-4c6d-8e7f-90a1b2c3d4e5
                """
                .formatted(classes + File.pathSeparator + resources),
        output.toString(StandardCharsets.UTF_8));
  }

  @Test
  void shouldColorTheColumnsOnTerminalOutput() throws Exception {
    deployHelloAppAndShop();
    PrintStream stream = new PrintStream(output, true, StandardCharsets.UTF_8);
    BBjShipCli colored = BBjShipCli.create(home).setInput(InputStream.nullInputStream())
        .setEnvironment(Map.of()).setOutput(stream).setError(stream).setAnsi(Ansi.ON)
        .setConnector((username, password) -> admin).build();

    int status = colored.run("apps", "list", "--verbose");

    String table = output.toString(StandardCharsets.UTF_8);
    assertEquals(0, status);
    assertTrue(table.contains("\u001b[1mNAME"));
    assertTrue(table.contains("\u001b[32mhello-app"));
    assertTrue(table.contains("\u001b[36m/webapp/hello-app"));
    assertTrue(table.contains("\u001b[2mdeployments/hello-app/"));
  }

  @Test
  void shouldSayWhenNoApplicationIsDeployed() {
    int status = cli.run("apps", "list");

    assertEquals(0, status);
    assertEquals(FACTORY_ACCOUNT_WARNING + "\n" + "No applications deployed\n",
        output.toString(StandardCharsets.UTF_8));
  }

  private void deployHelloAppAndShop() throws Exception {
    Path deployments = home.resolve("plugins/.webforj-bbjship-deployments/deployments");
    hello = deployments.resolve("hello-app/0b8f4f0e-3f5c-4a8e-9a49-5d1a6f3c2b10");
    shop = deployments.resolve("shop/6c1d2e3f-4a5b-4c6d-8e7f-90a1b2c3d4e5");
    DeploymentArchive.read(archive,
        Files.createDirectories(hello.getParent()).resolve(hello.getFileName()));
    DeploymentArchive.read(archive,
        Files.createDirectories(shop.getParent()).resolve(shop.getFileName()));
    BBjAdminAppDeploymentApplication helloRegistration =
        mock(BBjAdminAppDeploymentApplication.class);
    when(helloRegistration.getString(BBjAdminAppDeploymentApplication.NAME))
        .thenReturn("hello-app");
    when(helloRegistration.getString(BBjAdminAppDeploymentApplication.PROGRAM))
        .thenReturn(hello.resolve("bbj/webforj.bbj").toString());
    when(helloRegistration.getString(BBjAdminAppDeploymentApplication.CLASSPATH))
        .thenReturn("webforj_bbjship_hello-app");
    BBjAdminAppDeploymentApplication shopRegistration =
        mock(BBjAdminAppDeploymentApplication.class);
    when(shopRegistration.getString(BBjAdminAppDeploymentApplication.NAME)).thenReturn("shop");
    when(shopRegistration.getString(BBjAdminAppDeploymentApplication.PROGRAM))
        .thenReturn(shop.resolve("bbj/webforj.bbj").toString());
    when(shopRegistration.getString(BBjAdminAppDeploymentApplication.CLASSPATH))
        .thenReturn("webforj_bbjship_shop");
    when(configuration.getApplications())
        .thenReturn(new BBjAdminAdapterList<>(List.of(shopRegistration, helloRegistration)));
  }
}
