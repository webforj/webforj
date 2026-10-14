package com.webforj.bbjship.cli.command;

import static com.webforj.bbjship.foundation.model.BBjCredentials.FACTORY_ACCOUNT_WARNING;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
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
import java.io.ByteArrayOutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;
import picocli.CommandLine.Help.Ansi;

class AppsDeployCommandTest {

  private static final String HELLO_APP_NAME = "hello-app";
  private static final String DEPLOY_COMMAND = "deploy";
  private static final String PROOF_APP_NAME = "proof-app";

  private static final String PREVIOUS_ID = "0b8f4f0e-3f5c-4a8e-9a49-5d1a6f3c2b10";

  @TempDir
  Path temporary;

  private Path home;
  private Path app;
  private Path archive;
  private Path deployments;
  private ByteArrayOutputStream output;
  private BBjAdminBase admin;
  private BBjAdminAppDeploymentConfiguration configuration;
  private BBjAdminAppDeploymentApplication created;
  private BBjShipCli cli;

  @BeforeEach
  void initBBjShipWithHelloAppDeployed() throws Exception {
    home = Home.createCopy(temporary.resolve("bbx"));
    app = HelloApp.createCopy(temporary.resolve(HELLO_APP_NAME));
    archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
    deployments = home.resolve("plugins/.webforj-bbjship-deployments/deployments");
    Path previous =
        Files.createDirectories(deployments.resolve(HELLO_APP_NAME)).resolve(PREVIOUS_ID);
    DeploymentArchive.read(archive, previous);
    Files.writeString(deployments.resolve("hello-app/current"), PREVIOUS_ID);
    BBjServices services = new BBjServices();
    services.addBBjShipConfiguration(home);
    services.addClasspath("webforj_bbjship_hello-app", List.of(previous.toString()));
    admin = services.getAdmin();
    configuration = mock(BBjAdminAppDeploymentConfiguration.class);
    created = mock(BBjAdminAppDeploymentApplication.class);
    BBjAdminAppDeploymentApplication registration = mock(BBjAdminAppDeploymentApplication.class);
    when(admin.getRemoteConfiguration()).thenReturn(configuration);
    when(admin.getBBjProcesses()).thenReturn(List.of());
    when(configuration.getApplications())
        .thenReturn(new BBjAdminAdapterList<>(List.of(registration)));
    when(configuration.createApplication()).thenReturn(created);
    when(registration.getString(BBjAdminAppDeploymentApplication.NAME)).thenReturn(HELLO_APP_NAME);
    when(registration.getString(BBjAdminAppDeploymentApplication.PROGRAM))
        .thenReturn(previous.resolve("bbj/webforj.bbj").toString());
    when(registration.getString(BBjAdminAppDeploymentApplication.CLASSPATH))
        .thenReturn("webforj_bbjship_hello-app");
    when(registration.getProperties()).thenReturn(Map.of());
    when(registration.getArguments()).thenReturn(new BBjAdminAdapterList<>());
    when(registration.getJavaArguments()).thenReturn(new BBjAdminAdapterList<>());
    output = new ByteArrayOutputStream();
    Writer stream = new OutputStreamWriter(output, StandardCharsets.UTF_8);
    cli = BBjShipCli.create(new BBjShipPlugin(home)).setTerminal(new TerminalLines(stream))
        .setAnsi(Ansi.OFF).setSessions(login -> admin).build();
  }

  @Test
  void shouldDeployTheArchiveUnderItsFileName() throws Exception {
    Files.writeString(app.resolve("src/main/resources/webforj.conf"),
        "webforj.entry = \"org.example.ProofApplication\"");
    Path proofApp = HelloApp.writeArchive(app, temporary.resolve("proof-app.zip"));

    int status = cli.run("apps", DEPLOY_COMMAND, proofApp.toString());

    assertEquals(0, status);
    assertEquals(FACTORY_ACCOUNT_WARNING + "\n" + "Deployed proof-app /webapp/proof-app\n",
        output.toString(StandardCharsets.UTF_8));
    verify(created).setString(BBjAdminAppDeploymentApplication.NAME, PROOF_APP_NAME);
    verify(created).commit();
    verify(configuration, never()).removeApplication(PROOF_APP_NAME);
    verify(admin).release();
  }

  @Test
  void shouldDeployTheArchiveNamedRelativeToTheWorkingDirectoryOfTheTerminal() throws Exception {
    Files.writeString(app.resolve("src/main/resources/webforj.conf"),
        "webforj.entry = \"org.example.ProofApplication\"");
    Path folder = Files.createDirectories(temporary.resolve("builds"));
    HelloApp.writeArchive(app, folder.resolve("proof-app.zip"));
    Writer stream = new OutputStreamWriter(output, StandardCharsets.UTF_8);
    BBjShipCli inFolder = BBjShipCli.create(new BBjShipPlugin(home))
        .setTerminal(new TerminalLines(stream).setWorkingDirectory(temporary))
        .setSessions(login -> admin).build();

    int status = inFolder.run("apps", DEPLOY_COMMAND, "builds/proof-app.zip");

    assertEquals(0, status);
    assertEquals(FACTORY_ACCOUNT_WARNING + "\n" + "Deployed proof-app /webapp/proof-app\n",
        output.toString(StandardCharsets.UTF_8));
    verify(created).setString(BBjAdminAppDeploymentApplication.NAME, PROOF_APP_NAME);
  }

  @Test
  void shouldReplaceTheApplicationOfTheSameName() throws Exception {
    int status = cli.run("apps", DEPLOY_COMMAND, archive.toString());

    assertEquals(0, status);
    assertEquals(FACTORY_ACCOUNT_WARNING + "\n" + "Deployed hello-app /webapp/hello-app\n",
        output.toString(StandardCharsets.UTF_8));
    verify(configuration).removeApplication(HELLO_APP_NAME);
    verify(created).commit();
    assertNotEquals(PREVIOUS_ID, Files.readString(deployments.resolve("hello-app/current")));
    assertFalse(Files.exists(deployments.resolve(HELLO_APP_NAME).resolve(PREVIOUS_ID)));
  }

  @ParameterizedTest
  @MethodSource("getRefusedArchives")
  void shouldReportTheArchiveTheInstallerRefuses(String fileName, String message) throws Exception {
    Path refused = Files.copy(archive, temporary.resolve(fileName));

    int status = cli.run("apps", DEPLOY_COMMAND, refused.toString());

    assertEquals(1, status);
    assertEquals(FACTORY_ACCOUNT_WARNING + "\n" + "Command failed. " + message + "\n",
        output.toString(StandardCharsets.UTF_8));
    verify(configuration, never()).createApplication();
    verify(admin).release();
  }

  @ParameterizedTest
  @MethodSource("getRefusedPublishNames")
  void shouldRefuseThePublishNameOfTheFileNameBeforeOpeningTheSession(String fileName,
      String message) throws Exception {
    Path refused = Files.copy(archive, temporary.resolve(fileName));

    int status = cli.run("apps", DEPLOY_COMMAND, refused.toString());

    assertEquals(2, status);
    assertEquals(message + "\n", output.toString(StandardCharsets.UTF_8));
    verify(admin, never()).getClasspathNames();
    verify(admin, never()).getRemoteConfiguration();
  }

  @Test
  void shouldRefuseDevelopmentArchiveBeforeOpeningTheSession() throws Exception {
    Path development = Files.copy(archive, temporary.resolve("development-app.zip"));
    try (FileSystem zip = FileSystems.newFileSystem(development)) {
      Files.delete(zip.getPath("application.jar"));
      Files.writeString(zip.getPath("development.properties"), """
          classFolder=%s
          """.formatted(app.resolve("src/main/resources").toString().replace("\\", "\\\\")));
    }

    int status = cli.run("apps", DEPLOY_COMMAND, development.toString());

    assertEquals(2, status);
    assertEquals("Development archives are deployed by the watch, not by apps deploy\n",
        output.toString(StandardCharsets.UTF_8));
    verify(admin, never()).getClasspathNames();
    verify(admin, never()).getRemoteConfiguration();
  }

  @Test
  void shouldRefuseAnArchiveThatDoesNotExist() throws Exception {
    String missing = temporary.resolve("missing.zip").toString();

    int status = cli.run("apps", DEPLOY_COMMAND, missing);

    assertEquals(2, status);
    assertEquals("Deployment archive " + missing + " does not exist\n",
        output.toString(StandardCharsets.UTF_8));
    verify(admin, never()).getRemoteConfiguration();
  }

  @Test
  void shouldRefuseAnArchiveThatCannotBeRead() throws Exception {
    Files.setPosixFilePermissions(archive, Set.of());

    int status = cli.run("apps", DEPLOY_COMMAND, archive.toString());

    assertEquals(2, status);
    assertEquals("Deployment archive " + archive + " cannot be read\n",
        output.toString(StandardCharsets.UTF_8));
    verify(admin, never()).getRemoteConfiguration();
  }

  @ParameterizedTest
  @ValueSource(strings = {"hello-app.jar", HELLO_APP_NAME, ""})
  void shouldRefusePathWithoutZipExtension(String path) throws Exception {
    int status = cli.run("apps", DEPLOY_COMMAND, path);

    assertEquals(2, status);
    assertEquals("Specify the path of a deployment archive ending in .zip\n",
        output.toString(StandardCharsets.UTF_8));
    verify(admin, never()).getRemoteConfiguration();
  }

  @Test
  void shouldRefuseMoreThanOneArchive() throws Exception {
    int status = cli.run("apps", DEPLOY_COMMAND, archive.toString(), archive.toString());

    assertEquals(2, status);
    assertEquals("Specify one deployment archive\n", output.toString(StandardCharsets.UTF_8));
    verify(admin, never()).getRemoteConfiguration();
  }

  private static Stream<Arguments> getRefusedArchives() {
    return Stream.of(Arguments.of("second-app.zip",
        "Entry class org.example.Application is already published as hello-app. "
            + "Undeploy hello-app first"));
  }

  private static Stream<Arguments> getRefusedPublishNames() {
    return Stream.of(Arguments.of("My App.zip",
        "Publish name My App is invalid. Use a name that starts with a letter or digit, holds only "
            + "letters, digits, underscores, dots and hyphens, and is not a reserved device name "
            + "such as con or nul"),
        Arguments.of("webapp.zip", "The publish name webapp is reserved by BBjServices for its own "
            + "web paths. Choose another publish name"));
  }
}
