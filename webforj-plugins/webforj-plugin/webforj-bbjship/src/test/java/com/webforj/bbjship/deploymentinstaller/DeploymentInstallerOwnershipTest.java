package com.webforj.bbjship.deploymentinstaller;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
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
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.MethodSource;

final class DeploymentInstallerOwnershipTest {

  private static final String CLASSPATH_NAME = "webforj_bbjship_hello-app";

  @TempDir
  Path temporary;

  private Path app;
  private Path archive;
  private Path root;
  private Path previous;
  private BBjAdminBase admin;
  private BBjAdminAppDeploymentConfiguration configuration;
  private BBjAdminAppDeploymentApplication registered;

  @BeforeEach
  void initServerWithHelloAppPublished() throws Exception {
    app = HelloApp.createCopy(temporary.resolve("hello-app"));
    archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
    root = temporary.resolve("deployments");
    previous =
        Files.createDirectories(root.resolve("hello-app")).resolve(UUID.randomUUID().toString());
    DeploymentArchive.read(archive, previous);
    admin = mock(BBjAdminBase.class);
    configuration = mock(BBjAdminAppDeploymentConfiguration.class);
    registered = mock(BBjAdminAppDeploymentApplication.class);
    when(admin.getRemoteConfiguration()).thenReturn(configuration);
    when(admin.getClasspathNames()).thenReturn(new BBjAdminAdapterList<>(List.of(CLASSPATH_NAME)));
    when(configuration.getApplications())
        .thenReturn(new BBjAdminAdapterList<>(List.of(registered)));
    when(registered.getString(BBjAdminAppDeploymentApplication.NAME)).thenReturn("hello-app");
    when(registered.getString(BBjAdminAppDeploymentApplication.PROGRAM))
        .thenReturn(previous.resolve("bbj/webforj.bbj").toString());
    when(registered.getString(BBjAdminAppDeploymentApplication.CLASSPATH))
        .thenReturn(CLASSPATH_NAME);
  }

  @ParameterizedTest
  @MethodSource("getInvalidNames")
  void shouldRejectAnInvalidNameWithoutReadingTheServerOrTouchingFiles(String name)
      throws Exception {
    String destination = root.toString();
    InputStream upload = new ByteArrayInputStream(Files.readAllBytes(archive));

    IOException removal = assertThrows(IOException.class,
        () -> DeploymentInstaller.undeploy(name, destination, admin));
    IOException deployment = assertThrows(IOException.class,
        () -> DeploymentInstaller.deploy(upload, name, destination, admin));

    assertEquals("BBj deployment requires a valid publish name", removal.getMessage());
    assertEquals("BBj deployment requires a valid publish name", deployment.getMessage());
    verify(admin, never()).getRemoteConfiguration();
    verify(admin, never()).setClasspath(anyString(), any());
    verify(admin, never()).commit();
    try (Stream<Path> files = Files.walk(temporary)) {
      assertEquals(List.of(), files.filter(file -> file.toString().contains("escape")).toList());
    }

    try (Stream<Path> folders = Files.list(root)) {
      assertEquals(List.of(root.resolve("hello-app")), folders.toList());
    }
  }

  @ParameterizedTest
  @MethodSource("getForeignRegistrations")
  void shouldRefuseToUndeployAnApplicationThisBbjShipDoesNotOwn(String program, String classpath)
      throws Exception {
    Path directory = previous;
    when(registered.getString(BBjAdminAppDeploymentApplication.PROGRAM))
        .thenReturn(program == null ? null
            : program.replace("<deployment>", directory.toString())
                .replace("<root>", root.toString()).replace("<temporary>", temporary.toString()));
    when(registered.getString(BBjAdminAppDeploymentApplication.CLASSPATH)).thenReturn(classpath);
    String destination = root.toString();

    IOException failure = assertThrows(IOException.class,
        () -> DeploymentInstaller.undeploy("hello-app", destination, admin));

    assertEquals(
        "No BBj application named hello-app was deployed by BBjShip. " + "Check the publish name",
        failure.getMessage());
    verify(configuration, never()).removeApplication(anyString());
    verify(admin, never()).setClasspath(anyString(), any());
    verify(admin, never()).commit();
    verify(admin, never()).getBBjProcesses();
    assertArrayEquals(Files.readAllBytes(app.resolve("target/hello-app-1.0-SNAPSHOT.jar")),
        Files.readAllBytes(directory.resolve("application.jar")));
    try (Stream<Path> directories = Files.list(root.resolve("hello-app"))) {
      assertEquals(List.of(directory), directories.toList());
    }
  }

  @Test
  void shouldRefuseToReplaceAnApplicationOfAnotherClasspathBeforeAnyChange() throws Exception {
    when(registered.getString(BBjAdminAppDeploymentApplication.CLASSPATH))
        .thenReturn("foreign_classpath");
    InputStream upload = new ByteArrayInputStream(Files.readAllBytes(archive));
    String destination = root.toString();

    IOException failure = assertThrows(IOException.class,
        () -> DeploymentInstaller.deploy(upload, "hello-app", destination, admin));

    assertEquals("Published BBj application hello-app is not owned by BBjShip. "
        + "Choose another publish name", failure.getMessage());
    verify(configuration, never()).removeApplication(anyString());
    verify(admin, never()).setClasspath(anyString(), any());
    verify(admin, never()).commit();
    try (Stream<Path> directories = Files.list(root.resolve("hello-app"))) {
      assertEquals(List.of(previous), directories.toList());
    }
  }

  @Test
  void shouldRefuseNameThatDiffersOnlyByLetterCaseBeforeAnyChange() throws Exception {
    InputStream upload = new ByteArrayInputStream(Files.readAllBytes(archive));
    String destination = root.toString();

    IOException failure = assertThrows(IOException.class,
        () -> DeploymentInstaller.deploy(upload, "Hello-App", destination, admin));

    assertEquals("Published BBj application hello-app differs from Hello-App only by letter case. "
        + "Choose another publish name", failure.getMessage());
    verify(configuration, never()).createApplication();
    verify(admin, never()).setClasspath(anyString(), any());
    try (Stream<Path> folders = Files.list(root)) {
      assertEquals(List.of(root.resolve(".lock"), root.resolve("hello-app")),
          folders.sorted().toList());
    }

    try (Stream<Path> directories = Files.list(root.resolve("hello-app"))) {
      assertEquals(List.of(previous), directories.toList());
    }
  }

  @Test
  void shouldClaimTheClasspathOfTheNameWithoutPublishedApplication() throws Exception {
    when(configuration.getApplications()).thenReturn(new BBjAdminAdapterList<>(List.of()));
    BBjAdminAppDeploymentApplication published = mock(BBjAdminAppDeploymentApplication.class);
    when(configuration.createApplication()).thenReturn(published);
    when(admin.getClasspath(CLASSPATH_NAME)).thenReturn(
        new BBjAdminAdapterList<>(List.of(previous.resolve("application.jar").toString())));
    InputStream upload = new ByteArrayInputStream(Files.readAllBytes(archive));
    String destination = root.toString();

    DeploymentResult result = assertDoesNotThrow(
        () -> DeploymentInstaller.deploy(upload, "hello-app", destination, admin));

    assertEquals(DeploymentResult.CREATED, result);
    verify(admin).setClasspath(eq(CLASSPATH_NAME), any());
    verify(admin).commit();
    verify(published).commit();
    verify(configuration, never()).removeApplication(anyString());
    String current = Files.readString(root.resolve("hello-app/current"));
    try (Stream<Path> directories = Files.list(root.resolve("hello-app"))) {
      assertEquals(List.of(root.resolve("hello-app").resolve(current)),
          directories.filter(Files::isDirectory).toList());
    }
  }

  @ParameterizedTest
  @EnumSource(LinkedPath.class)
  void shouldRefuseProgramReachedThroughLinkOutsideTheDeploymentRoot(LinkedPath linked)
      throws Exception {
    Path otherRoot = Files.createDirectory(temporary.resolve("other-deployments"));
    Path deployment = otherRoot.resolve("hello-app").resolve(previous.getFileName());
    Path program = deployment.resolve("bbj/webforj.bbj");
    Path link = switch (linked) {
      case NAME_FOLDER -> otherRoot.resolve("hello-app");
      case DEPLOYMENT_FOLDER -> deployment;
      case LAUNCHER -> program;
    };
    Files.createDirectories(link.getParent());
    try {
      Files.createSymbolicLink(link, root.resolve(otherRoot.relativize(link)));
    } catch (UnsupportedOperationException unsupported) {
      assumeTrue(false, "The file system does not support symbolic links");
    }

    when(registered.getString(BBjAdminAppDeploymentApplication.PROGRAM))
        .thenReturn(program.toString());
    String destination = otherRoot.toString();

    DeploymentException failure = assertThrows(DeploymentException.class,
        () -> DeploymentInstaller.undeploy("hello-app", destination, admin));

    assertEquals(
        "No BBj application named hello-app was deployed by BBjShip. " + "Check the publish name",
        failure.getMessage());
    verify(configuration, never()).removeApplication(anyString());
    verify(admin, never()).setClasspath(anyString(), any());
    assertTrue(Files.exists(previous.resolve("application.jar")));
    assertTrue(Files.isSymbolicLink(link));
  }

  @Test
  void shouldRemoveTheLeftoverClasspathWhenOnlyOtherApplicationsArePublished() throws Exception {
    BBjAdminAppDeploymentApplication foreign = mock(BBjAdminAppDeploymentApplication.class);
    when(foreign.getString(BBjAdminAppDeploymentApplication.NAME)).thenReturn("foreign");
    when(configuration.getApplications()).thenReturn(new BBjAdminAdapterList<>(List.of(foreign)));
    when(admin.getClasspath(CLASSPATH_NAME)).thenReturn(
        new BBjAdminAdapterList<>(List.of(previous.resolve("application.jar").toString())));
    String destination = root.toString();

    assertDoesNotThrow(() -> DeploymentInstaller.undeploy("hello-app", destination, admin));

    verify(configuration, never()).removeApplication(anyString());
    verify(admin).setClasspath(CLASSPATH_NAME, null);
    verify(admin).commit();
    try (Stream<Path> directories = Files.list(root.resolve("hello-app"))) {
      assertEquals(List.of(previous), directories.toList());
    }
  }

  @Test
  void shouldRefuseUndeployWithNotFoundWithoutRegistrationOrClasspath() throws Exception {
    when(configuration.getApplications()).thenReturn(new BBjAdminAdapterList<>(List.of()));
    when(admin.getClasspathNames()).thenReturn(new BBjAdminAdapterList<>(List.of()));
    String destination = root.toString();

    IOException failure = assertThrows(IOException.class,
        () -> DeploymentInstaller.undeploy("hello-app", destination, admin));

    assertEquals(
        "No BBj application named hello-app was deployed by BBjShip. Check the publish name",
        failure.getMessage());
    verify(configuration, never()).removeApplication(anyString());
    verify(admin, never()).setClasspath(anyString(), any());
    verify(admin, never()).commit();
    verify(admin, never()).getBBjProcesses();
    try (Stream<Path> directories = Files.list(root.resolve("hello-app"))) {
      assertEquals(List.of(previous), directories.toList());
    }
  }

  private static Stream<Arguments> getInvalidNames() {
    return Stream.of(null, "", "../escape", "bad/escape", "-escape").map(Arguments::of);
  }

  private static Stream<Arguments> getForeignRegistrations() {
    return Stream.of(Arguments.of("<deployment>/bbj/webforj.bbj", "foreign_classpath"),
        Arguments.of(null, CLASSPATH_NAME), Arguments.of(" ", CLASSPATH_NAME),
        Arguments.of("/", CLASSPATH_NAME),
        Arguments.of("<deployment>/bbj/foreign.bbj", CLASSPATH_NAME),
        Arguments.of("<temporary>/foreign/bbj/webforj.bbj", CLASSPATH_NAME),
        Arguments.of("<root>/hello-app/invalid/bbj/webforj.bbj", CLASSPATH_NAME),
        Arguments.of("<root>/hello-app/1-1-1-1-1/bbj/webforj.bbj", CLASSPATH_NAME),
        Arguments.of("<root>/other/" + UUID.randomUUID() + "/bbj/webforj.bbj", CLASSPATH_NAME));
  }

  @Nested
  class DevelopmentRun {

    private Path classFolder;
    private Path development;

    @BeforeEach
    void initDevelopmentArchiveOfHelloAppWithDevelopmentRunsEnabled() throws Exception {
      classFolder = app.resolve("src/main/resources");
      development = HelloApp.writeArchive(app, temporary.resolve("development.zip"));
      try (FileSystem zip = FileSystems.newFileSystem(development)) {
        Files.delete(zip.getPath("application.jar"));
        Files.writeString(zip.getPath("development.properties"), """
            classFolder=%s
            """.formatted(classFolder.toString().replace("\\", "\\\\")));
      }

      Files.createFile(temporary.resolve("development.enabled"));
    }

    @Test
    void shouldRefuseToReplaceDevelopmentRunWithArchiveBeforeAnyChange() throws Exception {
      Files.writeString(previous.resolve("development"), classFolder.toString());
      InputStream upload = new ByteArrayInputStream(Files.readAllBytes(archive));
      String destination = root.toString();

      DeploymentException failure = assertThrows(DeploymentException.class,
          () -> DeploymentInstaller.deploy(upload, "hello-app", destination, admin));

      assertEquals("hello-app is a development run. Stop the watch first", failure.getMessage());
      verify(configuration, never()).createApplication();
      verify(admin, never()).setClasspath(anyString(), any());
      verify(admin, never()).commit();
      try (Stream<Path> directories = Files.list(root.resolve("hello-app"))) {
        assertEquals(List.of(previous), directories.toList());
      }

      assertEquals(classFolder.toString(), Files.readString(previous.resolve("development")));
    }

    @Test
    void shouldRefuseToReplaceArchiveDeploymentWithDevelopmentRunBeforeAnyChange()
        throws Exception {
      InputStream upload = new ByteArrayInputStream(Files.readAllBytes(development));
      String destination = root.toString();

      DeploymentException failure = assertThrows(DeploymentException.class,
          () -> DeploymentInstaller.deploy(upload, "hello-app", destination, admin));

      assertEquals("hello-app is deployed from an archive. Undeploy it first",
          failure.getMessage());
      verify(configuration, never()).createApplication();
      verify(admin, never()).setClasspath(anyString(), any());
      verify(admin, never()).commit();
      try (Stream<Path> directories = Files.list(root.resolve("hello-app"))) {
        assertEquals(List.of(previous), directories.toList());
      }
    }

    @Test
    void shouldReplaceDevelopmentRunWithDevelopmentRun() throws Exception {
      Files.writeString(previous.resolve("development"), classFolder.toString());
      BBjAdminAppDeploymentApplication published = mock(BBjAdminAppDeploymentApplication.class);
      when(configuration.createApplication()).thenReturn(published);
      when(admin.getClasspath(CLASSPATH_NAME)).thenReturn(
          new BBjAdminAdapterList<>(List.of(previous.resolve("dependencies").toString())));
      when(registered.getProperties()).thenReturn(Map.of());
      when(registered.getArguments()).thenReturn(new BBjAdminAdapterList<>(List.of()));
      when(registered.getJavaArguments()).thenReturn(new BBjAdminAdapterList<>(List.of()));
      InputStream upload = new ByteArrayInputStream(Files.readAllBytes(development));
      String destination = root.toString();

      DeploymentResult result = assertDoesNotThrow(
          () -> DeploymentInstaller.deploy(upload, "hello-app", destination, admin));

      assertEquals(DeploymentResult.REPLACED, result);
      verify(configuration).removeApplication("hello-app");
      verify(published).commit();
      Path current =
          root.resolve("hello-app").resolve(Files.readString(root.resolve("hello-app/current")));
      try (Stream<Path> directories = Files.list(root.resolve("hello-app"))) {
        assertEquals(List.of(current), directories.filter(Files::isDirectory).toList());
      }

      assertEquals(classFolder.toString(), Files.readString(current.resolve("development")));
    }

    @Test
    void shouldUndeployDevelopmentRun() throws Exception {
      Files.writeString(previous.resolve("development"), classFolder.toString());
      when(admin.getClasspath(CLASSPATH_NAME)).thenReturn(
          new BBjAdminAdapterList<>(List.of(previous.resolve("dependencies").toString())));
      when(registered.getProperties()).thenReturn(Map.of());
      when(registered.getArguments()).thenReturn(new BBjAdminAdapterList<>(List.of()));
      when(registered.getJavaArguments()).thenReturn(new BBjAdminAdapterList<>(List.of()));
      String destination = root.toString();

      assertDoesNotThrow(() -> DeploymentInstaller.undeploy("hello-app", destination, admin));

      verify(configuration).removeApplication("hello-app");
      verify(admin).setClasspath(CLASSPATH_NAME, null);
      verify(admin).commit();
      assertFalse(Files.exists(root.resolve("hello-app")));
    }
  }

  private enum LinkedPath {
    NAME_FOLDER, DEPLOYMENT_FOLDER, LAUNCHER
  }
}
