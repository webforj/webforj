package com.webforj.bbjship.deploymentinstaller;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertLinesMatch;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.api.admin.BBjAdminAppDeploymentApplication;
import com.basis.api.admin.BBjAdminAppDeploymentConfiguration;
import com.basis.api.admin.BBjAdminBBjProcess;
import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.BBjAdminException;
import com.basis.api.admin.rest.adapter.BBjAdminAdapterList;
import com.webforj.bbjship.ClasspathRelease;
import com.webforj.bbjship.DeploymentInstaller;
import com.webforj.bbjship.DeploymentInstaller.DeploymentResult;
import com.webforj.plugin.foundation.bbj.DeploymentArchive;
import com.webforj.plugin.foundation.bbj.HelloApp;
import java.io.ByteArrayInputStream;
import java.io.Closeable;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.ArgumentCaptor;
import org.mockito.InOrder;

final class DeploymentInstallerPublishedApplicationTest {

  private static final String CLASSPATH_NAME = "webforj_bbjship_hello-app";

  @TempDir
  Path temporary;

  private Path archive;
  private Path root;
  private Path previous;
  private BBjAdminBase admin;
  private BBjAdminAppDeploymentConfiguration configuration;
  private BBjAdminAppDeploymentApplication published;
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
    published = mock(BBjAdminAppDeploymentApplication.class);
    List<String> classpath = List.of("(_webforj_bbjship)",
        previous.resolve("dependencies/00000-webforj-plugin-foundation-26.03-SNAPSHOT.jar")
            .toString(),
        previous.resolve("dependencies/00001-webforj-foundation-26.03-SNAPSHOT.jar").toString(),
        previous.resolve("dependencies/00002-webforj-dispatcher-26.03-SNAPSHOT.jar").toString(),
        previous.resolve("dependencies/00003-webforj-data-26.03-SNAPSHOT.jar").toString(),
        previous.resolve("application.jar").toString());
    when(admin.getRemoteConfiguration()).thenReturn(configuration);
    when(admin.getClasspathNames()).thenReturn(new BBjAdminAdapterList<>(List.of(CLASSPATH_NAME)));
    when(admin.getClasspath(CLASSPATH_NAME)).thenReturn(new BBjAdminAdapterList<>(classpath));
    when(configuration.createApplication()).thenReturn(published);
    registered = mock(BBjAdminAppDeploymentApplication.class);
    when(configuration.getApplications())
        .thenReturn(new BBjAdminAdapterList<>(List.of(registered)));
    when(registered.getString(BBjAdminAppDeploymentApplication.NAME)).thenReturn("hello-app");
    when(registered.getString(BBjAdminAppDeploymentApplication.PROGRAM))
        .thenReturn(previous.resolve("bbj/webforj.bbj").toString());
    when(registered.getString(BBjAdminAppDeploymentApplication.CLASSPATH))
        .thenReturn(CLASSPATH_NAME);
    when(registered.getArguments()).thenReturn(new BBjAdminAdapterList<>(List.of()));
    when(registered.getJavaArguments()).thenReturn(new BBjAdminAdapterList<>(List.of()));
  }

  @Test
  void shouldRegisterTheApplicationWithItsClasspathAndLauncher() throws Exception {
    when(configuration.getApplications()).thenReturn(new BBjAdminAdapterList<>(List.of()));
    when(admin.getClasspathNames()).thenReturn(new BBjAdminAdapterList<>(List.of()));
    Path first = temporary.resolve("first-deployments");
    InputStream upload = new ByteArrayInputStream(Files.readAllBytes(archive));
    String destination = first.toString();

    DeploymentResult result = assertDoesNotThrow(
        () -> DeploymentInstaller.deploy(upload, "hello-app", destination, admin));

    Path deployed;
    try (Stream<Path> directories = Files.list(first.resolve("hello-app"))) {
      deployed = directories.filter(Files::isDirectory).findFirst().orElseThrow();
    }

    assertEquals(DeploymentResult.CREATED, result);
    assertEquals(deployed.getFileName().toString(),
        Files.readString(first.resolve("hello-app/current")));
    ArgumentCaptor<List<String>> classpath = ArgumentCaptor.captor();
    verify(admin).setClasspath(eq(CLASSPATH_NAME), classpath.capture());
    String dependencies = Pattern.quote(deployed.resolve("dependencies") + File.separator);
    assertLinesMatch(
        List.of("(_webforj_bbjship)", dependencies + "00000-webforj-plugin-foundation-.+\\.jar",
            dependencies + "00001-webforj-foundation-.+\\.jar",
            dependencies + "00002-webforj-dispatcher-.+\\.jar",
            dependencies + "00003-webforj-data-.+\\.jar",
            deployed.resolve("application.jar").toString()),
        classpath.getValue());
    verify(admin).commit();
    verify(published).setString(BBjAdminAppDeploymentApplication.NAME, "hello-app");
    Path program = deployed.resolve("bbj/webforj.bbj");
    verify(published).setString(BBjAdminAppDeploymentApplication.PROGRAM, program.toString());
    verify(published).setString(BBjAdminAppDeploymentApplication.CLASSPATH, CLASSPATH_NAME);
    verify(published).setString(BBjAdminAppDeploymentApplication.WORKING_DIRECTORY,
        program.getParent().toString());
    verify(published).setBoolean(BBjAdminAppDeploymentApplication.EXE_ENABLED, false);
    verify(published).setBoolean(BBjAdminAppDeploymentApplication.BUI_ENABLED, false);
    verify(published).setBoolean(BBjAdminAppDeploymentApplication.DWC_ENABLED, true);
    verify(published).commit();
    verify(configuration, never()).removeApplication(anyString());
  }

  @Test
  void shouldReplaceThePublishedApplicationAndStopOnlyTheSessionsOfItsClasspath() throws Exception {
    BBjAdminBBjProcess owned = mock(BBjAdminBBjProcess.class);
    BBjAdminBBjProcess similar = mock(BBjAdminBBjProcess.class);
    BBjAdminBBjProcess foreign = mock(BBjAdminBBjProcess.class);
    when(owned.getString(BBjAdminBBjProcess.SSCP_NAME)).thenReturn(CLASSPATH_NAME);
    when(similar.getString(BBjAdminBBjProcess.SSCP_NAME)).thenReturn(CLASSPATH_NAME + "_other");
    when(foreign.getString(BBjAdminBBjProcess.SSCP_NAME)).thenReturn("webforj_other");
    when(admin.getBBjProcesses()).thenReturn(List.of(owned, similar, foreign));
    Files.writeString(root.resolve("hello-app/current"), previous.getFileName().toString());
    doAnswer(invocation -> {
      assertTrue(Files.exists(previous.resolve("application.jar")));
      assertEquals(previous.getFileName().toString(),
          Files.readString(root.resolve("hello-app/current")));
      return null;
    }).when(published).commit();
    InputStream upload = new ByteArrayInputStream(Files.readAllBytes(archive));
    String destination = root.toString();

    DeploymentResult result = assertDoesNotThrow(
        () -> DeploymentInstaller.deploy(upload, "hello-app", destination, admin));

    assertEquals(DeploymentResult.REPLACED, result);
    Path deployed;
    try (Stream<Path> directories = Files.list(root.resolve("hello-app"))) {
      deployed = directories.filter(Files::isDirectory).findFirst().orElseThrow();
    }

    ArgumentCaptor<List<String>> classpath = ArgumentCaptor.captor();
    InOrder order = inOrder(configuration, owned, admin, published);
    order.verify(configuration).removeApplication("hello-app");
    order.verify(owned).terminate();
    order.verify(admin).setClasspath(eq(CLASSPATH_NAME), classpath.capture());
    order.verify(admin).commit();
    order.verify(published).commit();
    String dependencies = Pattern.quote(deployed.resolve("dependencies") + File.separator);
    assertLinesMatch(
        List.of("(_webforj_bbjship)", dependencies + "00000-webforj-plugin-foundation-.+\\.jar",
            dependencies + "00001-webforj-foundation-.+\\.jar",
            dependencies + "00002-webforj-dispatcher-.+\\.jar",
            dependencies + "00003-webforj-data-.+\\.jar",
            deployed.resolve("application.jar").toString()),
        classpath.getValue());
    verify(similar, never()).terminate();
    verify(foreign, never()).terminate();
    assertFalse(Files.exists(previous));
    assertTrue(Files.isRegularFile(deployed.resolve("bbj/webforj.bbj")));
    assertEquals(deployed.getFileName().toString(),
        Files.readString(root.resolve("hello-app/current")));
  }

  @Test
  void shouldUndeployThePublishedApplicationItsClasspathSessionsAndFolderOnly() throws Exception {
    BBjAdminBBjProcess owned = mock(BBjAdminBBjProcess.class);
    BBjAdminBBjProcess foreign = mock(BBjAdminBBjProcess.class);
    when(owned.getString(BBjAdminBBjProcess.SSCP_NAME)).thenReturn(CLASSPATH_NAME);
    when(foreign.getString(BBjAdminBBjProcess.SSCP_NAME)).thenReturn("webforj_other");
    when(admin.getBBjProcesses()).thenReturn(List.of(owned, foreign));
    Path other =
        Files.createDirectories(root.resolve("other")).resolve(UUID.randomUUID().toString());
    DeploymentArchive.read(archive, other);
    Files.writeString(root.resolve("hello-app/current"), previous.getFileName().toString());
    String destination = root.toString();

    assertDoesNotThrow(() -> DeploymentInstaller.undeploy("hello-app", destination, admin));

    InOrder order = inOrder(configuration, owned, admin);
    order.verify(configuration).removeApplication("hello-app");
    order.verify(owned).terminate();
    order.verify(admin).setClasspath(CLASSPATH_NAME, null);
    order.verify(admin).commit();
    verify(foreign, never()).terminate();
    try (Stream<Path> folders = Files.list(root)) {
      assertEquals(List.of(root.resolve(".lock"), root.resolve("other")),
          folders.sorted().toList());
    }

    assertTrue(Files.isRegularFile(other.resolve("application.jar")));
  }

  @Test
  void shouldReleaseTheFilesOfTheClasspathAfterItsSessionsEndedAndBeforeTheRemoval()
      throws Exception {
    BBjAdminBBjProcess owned = mock(BBjAdminBBjProcess.class);
    when(owned.getString(BBjAdminBBjProcess.SSCP_NAME)).thenReturn(CLASSPATH_NAME);
    when(admin.getBBjProcesses()).thenReturn(List.of(owned));
    Files.writeString(root.resolve("hello-app/current"), previous.getFileName().toString());
    ClasspathRelease release = mock(ClasspathRelease.class);
    Closeable hold = mock(Closeable.class);
    when(release.find(CLASSPATH_NAME)).thenReturn(hold);
    doAnswer(call -> {
      assertTrue(Files.isRegularFile(previous.resolve("application.jar")));
      return null;
    }).when(hold).close();
    String destination = root.toString();

    Optional<String> kept = assertDoesNotThrow(
        () -> DeploymentInstaller.undeploy("hello-app", destination, admin, release));

    assertEquals(Optional.empty(), kept);
    InOrder order = inOrder(release, configuration, owned, admin, hold);
    order.verify(release).find(CLASSPATH_NAME);
    order.verify(configuration).removeApplication("hello-app");
    order.verify(owned).terminate();
    order.verify(admin).setClasspath(CLASSPATH_NAME, null);
    order.verify(admin).commit();
    order.verify(hold).close();
    assertFalse(Files.exists(root.resolve("hello-app")));
  }

  @Test
  void shouldReleaseTheFilesWhenTheUnregistrationFails() throws Exception {
    Files.writeString(root.resolve("hello-app/current"), previous.getFileName().toString());
    doThrow(new BBjAdminException("refused")).when(admin).setClasspath(CLASSPATH_NAME, null);
    ClasspathRelease release = mock(ClasspathRelease.class);
    Closeable hold = mock(Closeable.class);
    when(release.find(CLASSPATH_NAME)).thenReturn(hold);
    String destination = root.toString();

    assertThrows(IOException.class,
        () -> DeploymentInstaller.undeploy("hello-app", destination, admin, release));

    verify(hold).close();
    assertTrue(Files.isRegularFile(previous.resolve("application.jar")));
  }

  @Test
  void shouldRemoveTheFolderWhenTheReleaseFails() throws Exception {
    Files.writeString(root.resolve("hello-app/current"), previous.getFileName().toString());
    ClasspathRelease release = mock(ClasspathRelease.class);
    Closeable hold = mock(Closeable.class);
    when(release.find(CLASSPATH_NAME)).thenReturn(hold);
    doThrow(new IOException("already closed")).when(hold).close();
    String destination = root.toString();

    Optional<String> kept = assertDoesNotThrow(
        () -> DeploymentInstaller.undeploy("hello-app", destination, admin, release));

    assertEquals(Optional.empty(), kept);
    assertFalse(Files.exists(root.resolve("hello-app")));
  }

  @Test
  void shouldUndeployAndKeepTheFolderWhoseFilesCannotBeRemoved() throws Exception {
    assumeTrue(root.getFileSystem().supportedFileAttributeViews().contains("posix"),
        "The removal failure needs a posix file system");
    Files.writeString(root.resolve("hello-app/current"), previous.getFileName().toString());
    assertTrue(previous.resolve("bbj").toFile().setWritable(false));
    String destination = root.toString();

    Optional<String> kept;
    try {
      kept =
          assertDoesNotThrow(() -> DeploymentInstaller.undeploy("hello-app", destination, admin));
    } finally {
      assertTrue(previous.resolve("bbj").toFile().setWritable(true));
    }

    assertEquals(
        Optional
            .of("Not removed: " + root.toRealPath().resolve("hello-app") + ". " + root.toRealPath()
                .resolve("hello-app").resolve(previous.getFileName()).resolve("bbj/webforj.bbj")),
        kept);
    verify(configuration).removeApplication("hello-app");
    verify(admin).setClasspath(CLASSPATH_NAME, null);
    verify(admin).commit();
    assertTrue(Files.isRegularFile(previous.resolve("bbj/webforj.bbj")));
  }

  @ParameterizedTest
  @ValueSource(strings = {"", "bbj/webforj.bbj"})
  void shouldReplaceThePublishedApplicationWhoseDeploymentIsGone(String removed) throws Exception {
    Files.writeString(root.resolve("hello-app/current"), previous.getFileName().toString());
    Files.move(previous.resolve(removed), temporary.resolve("gone"));
    InputStream upload = new ByteArrayInputStream(Files.readAllBytes(archive));
    String destination = root.toString();

    DeploymentResult result = assertDoesNotThrow(
        () -> DeploymentInstaller.deploy(upload, "hello-app", destination, admin));

    assertEquals(DeploymentResult.REPLACED, result);
    verify(configuration).removeApplication("hello-app");
    verify(published).commit();
    Path deployed;
    try (Stream<Path> directories = Files.list(root.resolve("hello-app"))) {
      deployed = directories.filter(Files::isDirectory).findFirst().orElseThrow();
    }

    assertEquals(deployed.getFileName().toString(),
        Files.readString(root.resolve("hello-app/current")));
  }

  @ParameterizedTest
  @ValueSource(strings = {"", "bbj/webforj.bbj"})
  void shouldUndeployThePublishedApplicationWhoseDeploymentIsGone(String removed) throws Exception {
    Files.writeString(root.resolve("hello-app/current"), previous.getFileName().toString());
    Files.move(previous.resolve(removed), temporary.resolve("gone"));
    String destination = root.toString();

    assertDoesNotThrow(() -> DeploymentInstaller.undeploy("hello-app", destination, admin));

    verify(configuration).removeApplication("hello-app");
    verify(admin).setClasspath(CLASSPATH_NAME, null);
    verify(admin).commit();
    try (Stream<Path> folders = Files.list(root)) {
      assertEquals(List.of(root.resolve(".lock")), folders.toList());
    }
  }

  @Test
  void shouldReplaceWhenThePreviousFolderCannotBeRemovedAndRemoveItOnTheNextDeploy()
      throws Exception {
    Files.writeString(root.resolve("hello-app/current"), previous.getFileName().toString());
    assertTrue(previous.resolve("bbj").toFile().setWritable(false));
    InputStream firstUpload = new ByteArrayInputStream(Files.readAllBytes(archive));
    String destination = root.toString();

    DeploymentResult first = assertDoesNotThrow(
        () -> DeploymentInstaller.deploy(firstUpload, "hello-app", destination, admin));

    assertEquals(DeploymentResult.REPLACED, first);
    String firstId = Files.readString(root.resolve("hello-app/current"));
    try (Stream<Path> entries = Files.list(root.resolve("hello-app"))) {
      assertEquals(Set.of("current", firstId, previous.getFileName().toString()),
          entries.map(entry -> entry.getFileName().toString()).collect(Collectors.toSet()));
    }

    assertTrue(previous.resolve("bbj").toFile().setWritable(true));
    Path firstDeployed = root.resolve("hello-app").resolve(firstId);
    when(registered.getString(BBjAdminAppDeploymentApplication.PROGRAM))
        .thenReturn(firstDeployed.resolve("bbj/webforj.bbj").toString());
    InputStream secondUpload = new ByteArrayInputStream(Files.readAllBytes(archive));

    DeploymentResult second = assertDoesNotThrow(
        () -> DeploymentInstaller.deploy(secondUpload, "hello-app", destination, admin));

    assertEquals(DeploymentResult.REPLACED, second);
    String secondId = Files.readString(root.resolve("hello-app/current"));
    try (Stream<Path> entries = Files.list(root.resolve("hello-app"))) {
      assertEquals(Set.of("current", secondId),
          entries.map(entry -> entry.getFileName().toString()).collect(Collectors.toSet()));
    }
  }

  @Test
  void shouldResolveOnlyTheJarsDirectlyInsideTheDependenciesFolderInFileNameOrder()
      throws IOException {
    Path jar = previous.resolve("application.jar");
    Path dependencies = Files.createDirectories(temporary.resolve("deployment/dependencies"));
    Files.copy(jar, dependencies.resolve("00001-hello-app.jar"));
    Files.copy(jar, dependencies.resolve("00000-hello-app.jar"));
    Files.copy(jar, dependencies.resolve("00009-hello-app.zip"));
    Files.createDirectories(dependencies.resolve("nested"));
    Files.copy(jar, dependencies.resolve("nested/00009-hello-app.jar"));

    List<Path> jars = DeploymentInstaller.resolveDependencyJars(temporary.resolve("deployment"));

    assertEquals(List.of(dependencies.resolve("00000-hello-app.jar"),
        dependencies.resolve("00001-hello-app.jar")), jars);
  }
}
