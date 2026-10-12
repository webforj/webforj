package com.webforj.bbjship.deploymentinstaller;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.basis.api.admin.BBjAdminAppDeploymentApplication;
import com.basis.api.admin.BBjAdminAppDeploymentConfiguration;
import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.BBjAdminException;
import com.basis.api.admin.rest.adapter.BBjAdminAdapterList;
import com.webforj.bbjship.DeploymentInstaller;
import com.webforj.bbjship.model.PublishedApplication;
import com.webforj.plugin.foundation.bbj.DeploymentArchive;
import com.webforj.plugin.foundation.bbj.HelloApp;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.UUID;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

final class DeploymentInstallerListingTest {

  @TempDir
  Path temporary;

  private Path archive;
  private Path root;
  private Path previous;
  private BBjAdminBase admin;
  private BBjAdminAppDeploymentConfiguration configuration;
  private BBjAdminAppDeploymentApplication registered;

  @BeforeEach
  void initServerWithHelloAppPublished() throws Exception {
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
    archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
    root = temporary.resolve("deployments");
    previous =
        Files.createDirectories(root.resolve("hello-app")).resolve(UUID.randomUUID().toString());
    DeploymentArchive.read(archive, previous);
    admin = mock(BBjAdminBase.class);
    configuration = mock(BBjAdminAppDeploymentConfiguration.class);
    registered = mock(BBjAdminAppDeploymentApplication.class);
    when(admin.getRemoteConfiguration()).thenReturn(configuration);
    when(configuration.getApplications())
        .thenReturn(new BBjAdminAdapterList<>(List.of(registered)));
    when(registered.getString(BBjAdminAppDeploymentApplication.NAME)).thenReturn("hello-app");
    when(registered.getString(BBjAdminAppDeploymentApplication.PROGRAM))
        .thenReturn(previous.resolve("bbj/webforj.bbj").toString());
    when(registered.getString(BBjAdminAppDeploymentApplication.CLASSPATH))
        .thenReturn("webforj_bbjship_hello-app");
  }

  @Test
  void shouldListTheOwnedApplicationsSortedByName() throws Exception {
    Path alpha =
        Files.createDirectories(root.resolve("alpha")).resolve(UUID.randomUUID().toString());
    DeploymentArchive.read(archive, alpha);
    BBjAdminAppDeploymentApplication alphaRegistration =
        mock(BBjAdminAppDeploymentApplication.class);
    when(alphaRegistration.getString(BBjAdminAppDeploymentApplication.NAME)).thenReturn("alpha");
    when(alphaRegistration.getString(BBjAdminAppDeploymentApplication.PROGRAM))
        .thenReturn(alpha.resolve("bbj/webforj.bbj").toString());
    when(alphaRegistration.getString(BBjAdminAppDeploymentApplication.CLASSPATH))
        .thenReturn("webforj_bbjship_alpha");
    when(configuration.getApplications())
        .thenReturn(new BBjAdminAdapterList<>(List.of(registered, alphaRegistration)));

    List<PublishedApplication> applications =
        DeploymentInstaller.findApplications(root.toString(), admin);

    assertEquals(List.of("alpha", "hello-app"),
        applications.stream().map(PublishedApplication::getName).toList());
    assertEquals(List.of(alpha.toRealPath(), previous.toRealPath()),
        applications.stream().map(PublishedApplication::getDirectory).toList());
  }

  @Test
  void shouldListTheClassFolderOfDevelopmentRun() throws Exception {
    Path classFolder = temporary.resolve("hello-app/src/main/resources");
    Files.writeString(previous.resolve("development"), classFolder.toString());

    List<PublishedApplication> applications =
        DeploymentInstaller.findApplications(root.toString(), admin);

    assertEquals(List.of(List.of(classFolder)),
        applications.stream().map(PublishedApplication::getClassFolders).toList());
  }

  @Test
  void shouldListEveryClassFolderOfDevelopmentRunInOrder() throws Exception {
    Path classes = temporary.resolve("hello-app/src/main/java");
    Path resources = temporary.resolve("hello-app/src/main/resources");
    Files.writeString(previous.resolve("development"), """
        %s
        %s
        """.formatted(classes, resources));

    List<PublishedApplication> applications =
        DeploymentInstaller.findApplications(root.toString(), admin);

    assertEquals(List.of(List.of(classes, resources)),
        applications.stream().map(PublishedApplication::getClassFolders).toList());
  }

  @Test
  void shouldSkipAnApplicationOfAnotherClasspath() throws Exception {
    when(registered.getString(BBjAdminAppDeploymentApplication.CLASSPATH))
        .thenReturn("foreign_classpath");

    List<PublishedApplication> applications =
        DeploymentInstaller.findApplications(root.toString(), admin);

    assertTrue(applications.isEmpty());
  }

  @Test
  void shouldListNothingWhenNoApplicationIsRegistered() throws Exception {
    when(configuration.getApplications()).thenReturn(new BBjAdminAdapterList<>(List.of()));

    List<PublishedApplication> applications =
        DeploymentInstaller.findApplications(root.toString(), admin);

    assertTrue(applications.isEmpty());
  }

  @Test
  void shouldSkipRegisteredApplicationWithoutFolderAndFolderWithoutRegistration() throws Exception {
    Files.createDirectories(root.resolve("orphan"));
    Files.createDirectories(root.resolve(".staging"));
    BBjAdminAppDeploymentApplication missing = mock(BBjAdminAppDeploymentApplication.class);
    when(missing.getString(BBjAdminAppDeploymentApplication.NAME)).thenReturn("missing");
    when(configuration.getApplications())
        .thenReturn(new BBjAdminAdapterList<>(List.of(registered, missing)));

    List<PublishedApplication> applications =
        DeploymentInstaller.findApplications(root.toString(), admin);

    assertEquals(List.of("hello-app"),
        applications.stream().map(PublishedApplication::getName).toList());
  }

  @Test
  void shouldListNothingWithoutDeploymentRoot() throws Exception {
    List<PublishedApplication> applications =
        DeploymentInstaller.findApplications(temporary.resolve("missing").toString(), admin);

    assertTrue(applications.isEmpty());
  }

  @Test
  void shouldFindClassFoldersAsApplicationSources() throws Exception {
    Path classes = temporary.resolve("classes");
    Path resources = temporary.resolve("resources");
    Files.writeString(previous.resolve("development"), classes + "\n" + resources + "\n");

    List<Path> sources = DeploymentInstaller.findApplicationSources(previous);

    assertEquals(List.of(classes, resources), sources);
  }

  @Test
  void shouldFindApplicationJarAsApplicationSources() throws Exception {
    List<Path> sources = DeploymentInstaller.findApplicationSources(previous);

    assertEquals(List.of(previous.resolve("application.jar")), sources);
  }

  @Test
  void shouldFindNoApplicationSourcesInEmptyDirectory() throws Exception {
    Path empty = Files.createDirectories(temporary.resolve("empty"));

    List<Path> sources = DeploymentInstaller.findApplicationSources(empty);

    assertTrue(sources.isEmpty());
  }

  @Test
  void shouldReportServerFailureWithMessage() throws Exception {
    BBjAdminException cause = new BBjAdminException("unavailable");
    when(configuration.getApplications()).thenThrow(cause);
    String destination = root.toString();

    IOException failure = assertThrows(IOException.class,
        () -> DeploymentInstaller.findApplications(destination, admin));

    assertEquals("BBj deployment failed for the BBjShip applications."
        + " Check deployment settings and the BBjServices log", failure.getMessage());
    assertSame(cause, failure.getCause());
  }
}
