package com.webforj.bbjship.deployer;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertLinesMatch;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;
import static org.mockito.ArgumentMatchers.anyList;
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
import com.webforj.bbjship.Deployer;
import com.webforj.bbjship.Deployer.DeploymentResult;
import com.webforj.bbjship.foundation.DeploymentArchive;
import com.webforj.bbjship.foundation.HelloApp;
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

final class DeployerPublishedApplicationTest {

  private static final String CLASSPATH_NAME = "webforj_bbjship_hello-app";
  private static final String NAME = "hello-app";
  private static final String CURRENT = "current";
  private static final String CURRENT_FILE = "hello-app/current";
  private static final String APPLICATION_JAR = "application.jar";
  private static final String PROGRAM = "bbj/webforj.bbj";
  private static final String SERVLET_CLASSPATH = "(_webforj_bbjship)";

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
    Path app = HelloApp.createCopy(temporary.resolve(NAME));
    archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
    root = temporary.resolve("deployments");
    previous = Files.createDirectories(root.resolve(NAME)).resolve(UUID.randomUUID().toString());
    DeploymentArchive.read(archive, previous);
    admin = mock(BBjAdminBase.class);
    configuration = mock(BBjAdminAppDeploymentConfiguration.class);
    published = mock(BBjAdminAppDeploymentApplication.class);
    List<String> classpath = List.of(SERVLET_CLASSPATH,
        previous.resolve("dependencies/00000-webforj-plugin-foundation-26.03-SNAPSHOT.jar")
            .toString(),
        previous.resolve("dependencies/00001-webforj-foundation-26.03-SNAPSHOT.jar").toString(),
        previous.resolve("dependencies/00002-webforj-dispatcher-26.03-SNAPSHOT.jar").toString(),
        previous.resolve("dependencies/00003-webforj-data-26.03-SNAPSHOT.jar").toString(),
        previous.resolve(APPLICATION_JAR).toString());
    when(admin.getRemoteConfiguration()).thenReturn(configuration);
    when(admin.getClasspathNames()).thenReturn(new BBjAdminAdapterList<>(List.of(CLASSPATH_NAME)));
    when(admin.getClasspath(CLASSPATH_NAME)).thenReturn(new BBjAdminAdapterList<>(classpath));
    when(configuration.createApplication()).thenReturn(published);
    registered = mock(BBjAdminAppDeploymentApplication.class);
    when(configuration.getApplications())
        .thenReturn(new BBjAdminAdapterList<>(List.of(registered)));
    when(registered.getString(BBjAdminAppDeploymentApplication.NAME)).thenReturn(NAME);
    when(registered.getString(BBjAdminAppDeploymentApplication.PROGRAM))
        .thenReturn(previous.resolve(PROGRAM).toString());
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
        () -> Deployer.deploy(upload, NAME, destination, admin, ClasspathRelease.NONE));

    Path deployed;
    try (Stream<Path> directories = Files.list(first.resolve(NAME))) {
      deployed = directories.filter(Files::isDirectory).findFirst().orElseThrow();
    }

    assertEquals(DeploymentResult.CREATED, result);
    assertEquals(deployed.getFileName().toString(), Files.readString(first.resolve(CURRENT_FILE)));
    ArgumentCaptor<List<String>> classpath = ArgumentCaptor.captor();
    verify(admin).setClasspath(eq(CLASSPATH_NAME), classpath.capture());
    String dependencies = Pattern.quote(deployed.resolve("dependencies") + File.separator);
    assertLinesMatch(List.of(SERVLET_CLASSPATH,
        dependencies + "00000-webforj-bbjship-foundation-.+\\.jar",
        dependencies + "00001-webforj-foundation-.+\\.jar",
        dependencies + "00002-webforj-dispatcher-.+\\.jar",
        dependencies + "00003-webforj-data-.+\\.jar", deployed.resolve(APPLICATION_JAR).toString()),
        classpath.getValue());
    verify(admin).commit();
    verify(published).setString(BBjAdminAppDeploymentApplication.NAME, NAME);
    Path program = deployed.resolve(PROGRAM);
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
    when(admin.getBBjProcesses()).thenReturn(List.of(owned, similar, foreign), List.of());
    Files.writeString(root.resolve(CURRENT_FILE), previous.getFileName().toString());
    doAnswer(invocation -> {
      assertTrue(Files.exists(previous.resolve(APPLICATION_JAR)));
      assertEquals(previous.getFileName().toString(), Files.readString(root.resolve(CURRENT_FILE)));
      return null;
    }).when(published).commit();
    InputStream upload = new ByteArrayInputStream(Files.readAllBytes(archive));
    String destination = root.toString();

    DeploymentResult result = assertDoesNotThrow(
        () -> Deployer.deploy(upload, NAME, destination, admin, ClasspathRelease.NONE));

    assertEquals(DeploymentResult.REPLACED, result);
    Path deployed;
    try (Stream<Path> directories = Files.list(root.resolve(NAME))) {
      deployed = directories.filter(Files::isDirectory).findFirst().orElseThrow();
    }

    ArgumentCaptor<List<String>> classpath = ArgumentCaptor.captor();
    InOrder order = inOrder(configuration, owned, admin, published);
    order.verify(configuration).removeApplication(NAME);
    order.verify(owned).terminate();
    order.verify(admin).setClasspath(eq(CLASSPATH_NAME), classpath.capture());
    order.verify(admin).commit();
    order.verify(published).commit();
    String dependencies = Pattern.quote(deployed.resolve("dependencies") + File.separator);
    assertLinesMatch(List.of(SERVLET_CLASSPATH,
        dependencies + "00000-webforj-bbjship-foundation-.+\\.jar",
        dependencies + "00001-webforj-foundation-.+\\.jar",
        dependencies + "00002-webforj-dispatcher-.+\\.jar",
        dependencies + "00003-webforj-data-.+\\.jar", deployed.resolve(APPLICATION_JAR).toString()),
        classpath.getValue());
    verify(similar, never()).terminate();
    verify(foreign, never()).terminate();
    assertFalse(Files.exists(previous));
    assertTrue(Files.isRegularFile(deployed.resolve(PROGRAM)));
    assertEquals(deployed.getFileName().toString(), Files.readString(root.resolve(CURRENT_FILE)));
  }

  @Test
  void shouldUndeployThePublishedApplicationItsClasspathSessionsAndFolderOnly() throws Exception {
    BBjAdminBBjProcess owned = mock(BBjAdminBBjProcess.class);
    BBjAdminBBjProcess foreign = mock(BBjAdminBBjProcess.class);
    when(owned.getString(BBjAdminBBjProcess.SSCP_NAME)).thenReturn(CLASSPATH_NAME);
    when(foreign.getString(BBjAdminBBjProcess.SSCP_NAME)).thenReturn("webforj_other");
    when(admin.getBBjProcesses()).thenReturn(List.of(owned, foreign), List.of());
    Path other =
        Files.createDirectories(root.resolve("other")).resolve(UUID.randomUUID().toString());
    DeploymentArchive.read(archive, other);
    Files.writeString(root.resolve(CURRENT_FILE), previous.getFileName().toString());
    String destination = root.toString();

    assertDoesNotThrow(() -> Deployer.undeploy(NAME, destination, admin, ClasspathRelease.NONE));

    InOrder order = inOrder(configuration, owned, admin);
    order.verify(configuration).removeApplication(NAME);
    order.verify(owned).terminate();
    order.verify(admin).setClasspath(CLASSPATH_NAME, null);
    order.verify(admin).commit();
    verify(foreign, never()).terminate();
    try (Stream<Path> folders = Files.list(root)) {
      assertEquals(List.of(root.resolve(".lock"), root.resolve("other")),
          folders.sorted().toList());
    }

    assertTrue(Files.isRegularFile(other.resolve(APPLICATION_JAR)));
  }

  @Test
  void shouldReleaseTheFilesOfThePreviousDeploymentWhenItIsReplaced() throws Exception {
    BBjAdminBBjProcess owned = mock(BBjAdminBBjProcess.class);
    when(owned.getString(BBjAdminBBjProcess.SSCP_NAME)).thenReturn(CLASSPATH_NAME);
    when(admin.getBBjProcesses()).thenReturn(List.of(owned), List.of());
    Files.writeString(root.resolve(CURRENT_FILE), previous.getFileName().toString());
    ClasspathRelease release = mock(ClasspathRelease.class);
    Closeable hold = mock(Closeable.class);
    when(release.find(CLASSPATH_NAME)).thenReturn(Optional.of(hold));
    InputStream upload = new ByteArrayInputStream(Files.readAllBytes(archive));
    String destination = root.toString();

    DeploymentResult result =
        assertDoesNotThrow(() -> Deployer.deploy(upload, NAME, destination, admin, release));

    assertEquals(DeploymentResult.REPLACED, result);
    InOrder order = inOrder(release, configuration, owned, hold, admin);
    order.verify(release).find(CLASSPATH_NAME);
    order.verify(configuration).removeApplication(NAME);
    order.verify(owned).terminate();
    order.verify(hold).close();
    order.verify(admin).setClasspath(eq(CLASSPATH_NAME), anyList());
    assertFalse(Files.exists(previous));
    try (Stream<Path> entries = Files.list(root.resolve(NAME))) {
      assertEquals(Set.of(CURRENT, Files.readString(root.resolve(CURRENT_FILE))),
          entries.map(entry -> entry.getFileName().toString()).collect(Collectors.toSet()));
    }
  }

  @Test
  void shouldReleaseTheFilesOfTheClasspathAfterItsSessionsEndedAndBeforeTheRemoval()
      throws Exception {
    BBjAdminBBjProcess owned = mock(BBjAdminBBjProcess.class);
    when(owned.getString(BBjAdminBBjProcess.SSCP_NAME)).thenReturn(CLASSPATH_NAME);
    when(admin.getBBjProcesses()).thenReturn(List.of(owned), List.of());
    Files.writeString(root.resolve(CURRENT_FILE), previous.getFileName().toString());
    ClasspathRelease release = mock(ClasspathRelease.class);
    Closeable hold = mock(Closeable.class);
    when(release.find(CLASSPATH_NAME)).thenReturn(Optional.of(hold));
    doAnswer(call -> {
      assertTrue(Files.isRegularFile(previous.resolve(APPLICATION_JAR)));
      return null;
    }).when(hold).close();
    String destination = root.toString();

    Optional<String> kept =
        assertDoesNotThrow(() -> Deployer.undeploy(NAME, destination, admin, release));

    assertEquals(Optional.empty(), kept);
    InOrder order = inOrder(release, configuration, owned, admin, hold);
    order.verify(release).find(CLASSPATH_NAME);
    order.verify(configuration).removeApplication(NAME);
    order.verify(owned).terminate();
    order.verify(admin).setClasspath(CLASSPATH_NAME, null);
    order.verify(admin).commit();
    order.verify(hold).close();
    assertFalse(Files.exists(root.resolve(NAME)));
  }

  @Test
  void shouldFailTheUndeployAndKeepTheClasspathWhenTheWaitForTheSessionsIsInterrupted()
      throws Exception {
    BBjAdminBBjProcess owned = mock(BBjAdminBBjProcess.class);
    when(owned.getString(BBjAdminBBjProcess.SSCP_NAME)).thenReturn(CLASSPATH_NAME);
    when(admin.getBBjProcesses()).thenReturn(List.of(owned));
    doAnswer(call -> {
      Thread.currentThread().interrupt();
      return null;
    }).when(owned).terminate();
    Files.writeString(root.resolve(CURRENT_FILE), previous.getFileName().toString());
    String destination = root.toString();

    IOException failure;
    try {
      failure = assertThrows(IOException.class,
          () -> Deployer.undeploy(NAME, destination, admin, ClasspathRelease.NONE));
    } finally {
      assertTrue(Thread.interrupted());
    }

    assertEquals(InterruptedException.class, failure.getCause().getClass());
    verify(admin, never()).setClasspath(CLASSPATH_NAME, null);
    assertTrue(Files.isRegularFile(previous.resolve(APPLICATION_JAR)));
  }

  @Test
  void shouldReleaseTheFilesWhenTheUnregistrationFails() throws Exception {
    Files.writeString(root.resolve(CURRENT_FILE), previous.getFileName().toString());
    doThrow(new BBjAdminException("refused")).when(admin).setClasspath(CLASSPATH_NAME, null);
    ClasspathRelease release = mock(ClasspathRelease.class);
    Closeable hold = mock(Closeable.class);
    when(release.find(CLASSPATH_NAME)).thenReturn(Optional.of(hold));
    String destination = root.toString();

    assertThrows(IOException.class, () -> Deployer.undeploy(NAME, destination, admin, release));

    verify(hold).close();
    assertTrue(Files.isRegularFile(previous.resolve(APPLICATION_JAR)));
  }

  @Test
  void shouldRemoveTheFolderWhenTheReleaseFails() throws Exception {
    Files.writeString(root.resolve(CURRENT_FILE), previous.getFileName().toString());
    ClasspathRelease release = mock(ClasspathRelease.class);
    Closeable hold = mock(Closeable.class);
    when(release.find(CLASSPATH_NAME)).thenReturn(Optional.of(hold));
    doThrow(new IOException("already closed")).when(hold).close();
    String destination = root.toString();

    Optional<String> kept =
        assertDoesNotThrow(() -> Deployer.undeploy(NAME, destination, admin, release));

    assertEquals(Optional.empty(), kept);
    assertFalse(Files.exists(root.resolve(NAME)));
  }

  @Test
  void shouldUndeployAndKeepTheFolderWhoseFilesCannotBeRemoved() throws Exception {
    assumeTrue(root.getFileSystem().supportedFileAttributeViews().contains("posix"),
        "The removal failure needs a posix file system");
    Files.writeString(root.resolve(CURRENT_FILE), previous.getFileName().toString());
    assertTrue(previous.resolve("bbj").toFile().setWritable(false));
    String destination = root.toString();

    Optional<String> kept;
    try {
      kept = assertDoesNotThrow(
          () -> Deployer.undeploy(NAME, destination, admin, ClasspathRelease.NONE));
    } finally {
      assertTrue(previous.resolve("bbj").toFile().setWritable(true));
    }

    assertEquals(
        Optional.of("Not removed: " + root.toRealPath().resolve(NAME) + ". "
            + root.toRealPath().resolve(NAME).resolve(previous.getFileName()).resolve(PROGRAM)),
        kept);
    verify(configuration).removeApplication(NAME);
    verify(admin).setClasspath(CLASSPATH_NAME, null);
    verify(admin).commit();
    assertTrue(Files.isRegularFile(previous.resolve(PROGRAM)));
  }

  @ParameterizedTest
  @ValueSource(strings = {"", PROGRAM})
  void shouldReplaceThePublishedApplicationWhoseDeploymentIsGone(String removed) throws Exception {
    Files.writeString(root.resolve(CURRENT_FILE), previous.getFileName().toString());
    Files.move(previous.resolve(removed), temporary.resolve("gone"));
    InputStream upload = new ByteArrayInputStream(Files.readAllBytes(archive));
    String destination = root.toString();

    DeploymentResult result = assertDoesNotThrow(
        () -> Deployer.deploy(upload, NAME, destination, admin, ClasspathRelease.NONE));

    assertEquals(DeploymentResult.REPLACED, result);
    verify(configuration).removeApplication(NAME);
    verify(published).commit();
    Path deployed;
    try (Stream<Path> directories = Files.list(root.resolve(NAME))) {
      deployed = directories.filter(Files::isDirectory).findFirst().orElseThrow();
    }

    assertEquals(deployed.getFileName().toString(), Files.readString(root.resolve(CURRENT_FILE)));
  }

  @ParameterizedTest
  @ValueSource(strings = {"", PROGRAM})
  void shouldUndeployThePublishedApplicationWhoseDeploymentIsGone(String removed) throws Exception {
    Files.writeString(root.resolve(CURRENT_FILE), previous.getFileName().toString());
    Files.move(previous.resolve(removed), temporary.resolve("gone"));
    String destination = root.toString();

    assertDoesNotThrow(() -> Deployer.undeploy(NAME, destination, admin, ClasspathRelease.NONE));

    verify(configuration).removeApplication(NAME);
    verify(admin).setClasspath(CLASSPATH_NAME, null);
    verify(admin).commit();
    try (Stream<Path> folders = Files.list(root)) {
      assertEquals(List.of(root.resolve(".lock")), folders.toList());
    }
  }

  @Test
  void shouldReplaceWhenThePreviousFolderCannotBeRemovedAndRemoveItOnTheNextDeploy()
      throws Exception {
    Files.writeString(root.resolve(CURRENT_FILE), previous.getFileName().toString());
    assertTrue(previous.resolve("bbj").toFile().setWritable(false));
    InputStream firstUpload = new ByteArrayInputStream(Files.readAllBytes(archive));
    String destination = root.toString();

    DeploymentResult first = assertDoesNotThrow(
        () -> Deployer.deploy(firstUpload, NAME, destination, admin, ClasspathRelease.NONE));

    assertEquals(DeploymentResult.REPLACED, first);
    String firstId = Files.readString(root.resolve(CURRENT_FILE));
    try (Stream<Path> entries = Files.list(root.resolve(NAME))) {
      assertEquals(Set.of(CURRENT, firstId, previous.getFileName().toString()),
          entries.map(entry -> entry.getFileName().toString()).collect(Collectors.toSet()));
    }

    assertTrue(previous.resolve("bbj").toFile().setWritable(true));
    Path firstDeployed = root.resolve(NAME).resolve(firstId);
    when(registered.getString(BBjAdminAppDeploymentApplication.PROGRAM))
        .thenReturn(firstDeployed.resolve(PROGRAM).toString());
    InputStream secondUpload = new ByteArrayInputStream(Files.readAllBytes(archive));

    DeploymentResult second = assertDoesNotThrow(
        () -> Deployer.deploy(secondUpload, NAME, destination, admin, ClasspathRelease.NONE));

    assertEquals(DeploymentResult.REPLACED, second);
    String secondId = Files.readString(root.resolve(CURRENT_FILE));
    try (Stream<Path> entries = Files.list(root.resolve(NAME))) {
      assertEquals(Set.of(CURRENT, secondId),
          entries.map(entry -> entry.getFileName().toString()).collect(Collectors.toSet()));
    }
  }
}
