package com.webforj.bbjship.deploymentinstaller;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.api.admin.BBjAdminAppDeploymentApplication;
import com.basis.api.admin.BBjAdminAppDeploymentConfiguration;
import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.rest.adapter.BBjAdminAdapterList;
import com.webforj.bbjship.DeploymentException;
import com.webforj.bbjship.DeploymentInstaller;
import com.webforj.bbjship.DeploymentInstaller.DeploymentResult;
import com.webforj.plugin.foundation.bbj.DeploymentArchive;
import com.webforj.plugin.foundation.bbj.HelloApp;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.UUID;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

final class DeploymentInstallerEntryTest {

  private static final String CLASSPATH_NAME = "webforj_bbjship_hello-app";

  @TempDir
  Path temporary;

  private Path app;
  private Path archive;
  private Path root;
  private Path previous;
  private BBjAdminBase admin;
  private BBjAdminAppDeploymentConfiguration configuration;

  @BeforeEach
  void initServerWithHelloAppPublished() throws Exception {
    app = HelloApp.createCopy(temporary.resolve("hello-app"));
    archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
    root = temporary.resolve("deployments");
    previous =
        Files.createDirectories(root.resolve("hello-app")).resolve(UUID.randomUUID().toString());
    DeploymentArchive.read(archive, previous);
    Files.writeString(root.resolve("hello-app/current"), previous.getFileName().toString());
    admin = mock(BBjAdminBase.class);
    configuration = mock(BBjAdminAppDeploymentConfiguration.class);
    BBjAdminAppDeploymentApplication registered = mock(BBjAdminAppDeploymentApplication.class);
    when(admin.getRemoteConfiguration()).thenReturn(configuration);
    when(admin.getClasspathNames()).thenReturn(new BBjAdminAdapterList<>(List.of(CLASSPATH_NAME)));
    when(admin.getClasspath(CLASSPATH_NAME)).thenReturn(new BBjAdminAdapterList<>(List.of()));
    when(configuration.getApplications())
        .thenReturn(new BBjAdminAdapterList<>(List.of(registered)));
    BBjAdminAppDeploymentApplication created = mock(BBjAdminAppDeploymentApplication.class);
    when(configuration.createApplication()).thenReturn(created);
    when(registered.getString(BBjAdminAppDeploymentApplication.NAME)).thenReturn("hello-app");
    when(registered.getString(BBjAdminAppDeploymentApplication.PROGRAM))
        .thenReturn(previous.resolve("bbj/webforj.bbj").toString());
    when(registered.getString(BBjAdminAppDeploymentApplication.CLASSPATH))
        .thenReturn(CLASSPATH_NAME);
    when(registered.getArguments()).thenReturn(new BBjAdminAdapterList<>(List.of()));
    when(registered.getJavaArguments()).thenReturn(new BBjAdminAdapterList<>(List.of()));
  }

  @Test
  void shouldRefuseTheEntryClassPublishedUnderAnotherNameBeforeAnyChange() throws Exception {
    InputStream upload = new ByteArrayInputStream(Files.readAllBytes(archive));
    String destination = root.toString();

    DeploymentException failure = assertThrows(DeploymentException.class,
        () -> DeploymentInstaller.deploy(upload, "second-app", destination, admin));

    assertEquals("Entry class org.example.Application is already published as hello-app. "
        + "Undeploy hello-app first", failure.getMessage());
    verify(configuration, never()).createApplication();
    verify(configuration, never()).removeApplication(anyString());
    verify(admin, never()).setClasspath(anyString(), any());
    verify(admin, never()).commit();
    try (Stream<Path> folders = Files.list(root)) {
      assertEquals(List.of(root.resolve(".lock"), root.resolve("hello-app")),
          folders.sorted().toList());
    }
  }

  @Test
  void shouldRefuseTheEntryClassOfDevelopmentRunPublishedUnderAnotherName() throws Exception {
    Files.delete(previous.resolve("application.jar"));
    Files.writeString(previous.resolve("development"),
        app.resolve("src/main/resources").toString());
    InputStream upload = new ByteArrayInputStream(Files.readAllBytes(archive));
    String destination = root.toString();

    DeploymentException failure = assertThrows(DeploymentException.class,
        () -> DeploymentInstaller.deploy(upload, "second-app", destination, admin));

    assertEquals("Entry class org.example.Application is already published as hello-app. "
        + "Undeploy hello-app first", failure.getMessage());
    verify(configuration, never()).createApplication();
  }

  @ParameterizedTest
  @ValueSource(strings = {"webforj.entry = \"org.example.Other\"", "webforj.quiet = true"})
  void shouldDeployWhenNoOtherNamePublishesTheEntryClass(String webforjConf) throws Exception {
    Files.writeString(app.resolve("src/main/resources/webforj.conf"), webforjConf);
    Path changed = HelloApp.writeArchive(app, temporary.resolve("changed.zip"));
    InputStream upload = new ByteArrayInputStream(Files.readAllBytes(changed));
    String destination = root.toString();

    DeploymentResult result = assertDoesNotThrow(
        () -> DeploymentInstaller.deploy(upload, "second-app", destination, admin));

    assertEquals(DeploymentResult.CREATED, result);
  }

  @ParameterizedTest
  @ValueSource(strings = {"", "application.jar"})
  void shouldSkipPublishedApplicationWhoseDeploymentIsGone(String removed) throws Exception {
    Files.move(previous.resolve(removed), temporary.resolve("gone"));
    InputStream upload = new ByteArrayInputStream(Files.readAllBytes(archive));
    String destination = root.toString();

    DeploymentResult result = assertDoesNotThrow(
        () -> DeploymentInstaller.deploy(upload, "second-app", destination, admin));

    assertEquals(DeploymentResult.CREATED, result);
  }
}
