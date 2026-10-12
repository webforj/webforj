package com.webforj.bbjship.deploymentinstaller;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
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
import com.webforj.bbjship.DeploymentException;
import com.webforj.bbjship.DeploymentInstaller;
import com.webforj.plugin.foundation.bbj.DeploymentArchive;
import com.webforj.plugin.foundation.bbj.HelloApp;
import java.io.ByteArrayInputStream;
import java.io.File;
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

final class DeploymentInstallerFailureTest {

  private static final String CLASSPATH_NAME = "webforj_bbjship_hello-app";

  @TempDir
  Path temporary;

  private Path app;
  private Path archive;
  private Path root;
  private Path previous;
  private List<String> previousClasspath;
  private Map<String, Object> previousProperties;
  private BBjAdminBase admin;
  private BBjAdminAppDeploymentConfiguration configuration;
  private BBjAdminAppDeploymentApplication replacement;
  private BBjAdminAppDeploymentApplication restored;

  @BeforeEach
  void initServerWithHelloAppPublished() throws Exception {
    app = HelloApp.createCopy(temporary.resolve("hello-app"));
    archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
    root = temporary.resolve("deployments");
    previous =
        Files.createDirectories(root.resolve("hello-app")).resolve(UUID.randomUUID().toString());
    DeploymentArchive.read(archive, previous);
    Files.writeString(root.resolve("hello-app/current"), previous.getFileName().toString());
    String program = previous.resolve("bbj/webforj.bbj").toString();
    previousClasspath = List.of("(_webforj_bbjship)",
        previous.resolve("dependencies/00000-webforj-plugin-foundation-26.03-SNAPSHOT.jar")
            .toString(),
        previous.resolve("dependencies/00001-webforj-foundation-26.03-SNAPSHOT.jar").toString(),
        previous.resolve("dependencies/00002-webforj-dispatcher-26.03-SNAPSHOT.jar").toString(),
        previous.resolve("dependencies/00003-webforj-data-26.03-SNAPSHOT.jar").toString(),
        previous.resolve("application.jar").toString());
    previousProperties = Map.of(BBjAdminAppDeploymentApplication.NAME, "hello-app",
        BBjAdminAppDeploymentApplication.PROGRAM, program,
        BBjAdminAppDeploymentApplication.CLASSPATH, CLASSPATH_NAME,
        BBjAdminAppDeploymentApplication.SHORTCUT_DESCRIPTION, "Hello App");
    admin = mock(BBjAdminBase.class);
    configuration = mock(BBjAdminAppDeploymentConfiguration.class);
    replacement = mock(BBjAdminAppDeploymentApplication.class);
    restored = mock(BBjAdminAppDeploymentApplication.class);
    when(admin.getRemoteConfiguration()).thenReturn(configuration);
    when(admin.getClasspathNames()).thenReturn(new BBjAdminAdapterList<>(List.of(CLASSPATH_NAME)));
    when(admin.getClasspath(CLASSPATH_NAME))
        .thenReturn(new BBjAdminAdapterList<>(previousClasspath));
    when(admin.getBBjProcesses()).thenReturn(List.of());
    when(configuration.createApplication()).thenReturn(restored);
    BBjAdminAppDeploymentApplication registered = mock(BBjAdminAppDeploymentApplication.class);
    when(configuration.getApplications())
        .thenReturn(new BBjAdminAdapterList<>(List.of(registered)));
    when(registered.getString(BBjAdminAppDeploymentApplication.NAME)).thenReturn("hello-app");
    when(registered.getString(BBjAdminAppDeploymentApplication.PROGRAM)).thenReturn(program);
    when(registered.getString(BBjAdminAppDeploymentApplication.CLASSPATH))
        .thenReturn(CLASSPATH_NAME);
    when(registered.getProperties()).thenReturn(previousProperties);
    when(registered.getArguments()).thenReturn(new BBjAdminAdapterList<>(List.of("--previous")));
    when(registered.getJavaArguments())
        .thenReturn(new BBjAdminAdapterList<>(List.of("-Dprevious=true")));
    when(restored.getArguments()).thenReturn(new BBjAdminAdapterList<>(List.of()));
    when(restored.getJavaArguments()).thenReturn(new BBjAdminAdapterList<>(List.of()));
  }

  @ParameterizedTest
  @MethodSource("getMutationFailures")
  void shouldRestoreThePublishedApplicationWhenOneChangeFails(Operation operation,
      FailurePoint point) throws Exception {
    BBjAdminException primary = new BBjAdminException("change failed");
    BBjAdminBBjProcess owned = mock(BBjAdminBBjProcess.class);
    BBjAdminBBjProcess foreign = mock(BBjAdminBBjProcess.class);
    when(owned.getString(BBjAdminBBjProcess.SSCP_NAME)).thenReturn(CLASSPATH_NAME);
    when(foreign.getString(BBjAdminBBjProcess.SSCP_NAME)).thenReturn("foreign_classpath");
    when(admin.getBBjProcesses()).thenReturn(List.of(owned, foreign));
    if (operation == Operation.DEPLOY) {
      when(configuration.createApplication()).thenReturn(replacement, restored);
    }

    switch (point) {
      case TERMINATE -> doThrow(primary).when(owned).terminate();
      case SET_CLASSPATH ->
        doThrow(primary).doNothing().when(admin).setClasspath(anyString(), any());
      case COMMIT -> doThrow(primary).doNothing().when(admin).commit();
      default -> throw new IllegalArgumentException(point.name());
    }

    InputStream upload = new ByteArrayInputStream(Files.readAllBytes(archive));
    String destination = root.toString();

    IOException failure = assertThrows(IOException.class,
        operation == Operation.DEPLOY
            ? () -> DeploymentInstaller.deploy(upload, "hello-app", destination, admin)
            : () -> DeploymentInstaller.undeploy("hello-app", destination, admin));

    assertSame(primary, failure.getCause());
    assertEquals(0, failure.getSuppressed().length);
    verify(admin).setClasspath(CLASSPATH_NAME, previousClasspath);
    verify(restored).setProperties(previousProperties);
    assertEquals(List.of("--previous"), restored.getArguments());
    assertEquals(List.of("-Dprevious=true"), restored.getJavaArguments());
    verify(restored).commit();
    verify(foreign, never()).terminate();
    assertArrayEquals(Files.readAllBytes(app.resolve("target/hello-app-1.0-SNAPSHOT.jar")),
        Files.readAllBytes(previous.resolve("application.jar")));
    try (Stream<Path> directories = Files.list(root.resolve("hello-app"))) {
      assertEquals(List.of(previous), directories.filter(Files::isDirectory).toList());
    }

    assertEquals(previous.getFileName().toString(),
        Files.readString(root.resolve("hello-app/current")));
  }

  @ParameterizedTest
  @EnumSource(RestorationPoint.class)
  void shouldFinishTheRestorationAndKeepEachOfItsFailures(RestorationPoint point) throws Exception {
    BBjAdminException primary = new BBjAdminException("publication failed");
    BBjAdminException cleanup = new BBjAdminException("restoration failed");
    when(configuration.createApplication()).thenReturn(replacement, restored);
    doThrow(primary).when(replacement).commit();
    switch (point) {
      case REMOVE_APPLICATION ->
        doNothing().doThrow(cleanup).when(configuration).removeApplication("hello-app");
      case SET_CLASSPATH ->
        doNothing().doThrow(cleanup).when(admin).setClasspath(anyString(), any());
      case COMMIT -> doNothing().doThrow(cleanup).when(admin).commit();
      case RESTORE_APPLICATION -> doThrow(cleanup).when(restored).commit();
      default -> throw new IllegalArgumentException(point.name());
    }

    InputStream upload = new ByteArrayInputStream(Files.readAllBytes(archive));
    String destination = root.toString();

    IOException failure = assertThrows(IOException.class,
        () -> DeploymentInstaller.deploy(upload, "hello-app", destination, admin));

    assertSame(primary, failure.getCause());
    assertEquals(List.of(cleanup), List.of(failure.getSuppressed()));
    verify(admin).setClasspath(CLASSPATH_NAME, previousClasspath);
    verify(restored).setProperties(previousProperties);
    assertEquals(List.of("--previous"), restored.getArguments());
    assertEquals(List.of("-Dprevious=true"), restored.getJavaArguments());
    verify(restored).commit();
    try (Stream<Path> directories = Files.list(root.resolve("hello-app"))) {
      assertEquals(List.of(previous), directories.filter(Files::isDirectory).toList());
    }

    assertEquals(previous.getFileName().toString(),
        Files.readString(root.resolve("hello-app/current")));
  }

  @ParameterizedTest
  @EnumSource(Operation.class)
  void shouldReportTheFailureToListTheApplicationsWithoutAnyChange(Operation operation)
      throws Exception {
    BBjAdminException primary = new BBjAdminException("listing failed");
    when(configuration.getApplications()).thenThrow(primary);
    InputStream upload = new ByteArrayInputStream(Files.readAllBytes(archive));
    String destination = root.toString();

    IOException failure = assertThrows(IOException.class,
        operation == Operation.DEPLOY
            ? () -> DeploymentInstaller.deploy(upload, "hello-app", destination, admin)
            : () -> DeploymentInstaller.undeploy("hello-app", destination, admin));

    assertSame(primary, failure.getCause());
    verify(configuration, never()).createApplication();
    verify(configuration, never()).removeApplication(anyString());
    verify(admin, never()).setClasspath(anyString(), any());
    verify(admin, never()).commit();
    verify(admin, never()).getBBjProcesses();
    try (Stream<Path> directories = Files.list(root.resolve("hello-app"))) {
      assertEquals(List.of(previous), directories.filter(Files::isDirectory).toList());
    }

    assertEquals(previous.getFileName().toString(),
        Files.readString(root.resolve("hello-app/current")));
  }

  @Test
  void shouldRemoveTheNewFolderAndRegistrationWhenTheFirstPublicationFails() throws Exception {
    BBjAdminException primary = new BBjAdminException("publication failed");
    when(configuration.getApplications()).thenReturn(new BBjAdminAdapterList<>(List.of()));
    when(admin.getClasspathNames()).thenReturn(new BBjAdminAdapterList<>(List.of()));
    when(configuration.createApplication()).thenReturn(replacement);
    doThrow(primary).when(replacement).commit();
    Path first = temporary.resolve("first-deployments");
    InputStream upload = new ByteArrayInputStream(Files.readAllBytes(archive));
    String destination = first.toString();

    IOException failure = assertThrows(IOException.class,
        () -> DeploymentInstaller.deploy(upload, "hello-app", destination, admin));

    assertEquals("BBj deployment failed for hello-app. Check deployment settings and the "
        + "BBjServices log", failure.getMessage());
    assertSame(primary, failure.getCause());
    verify(configuration).removeApplication("hello-app");
    verify(admin).setClasspath(CLASSPATH_NAME, null);
    try (Stream<Path> directories = Files.list(first)) {
      assertEquals(List.of(first.resolve(".lock")), directories.toList());
    }
  }

  @Test
  void shouldRestoreTheLeftoverClasspathWhenItsClaimFails() throws Exception {
    BBjAdminException primary = new BBjAdminException("publication failed");
    when(configuration.getApplications()).thenReturn(new BBjAdminAdapterList<>(List.of()));
    when(configuration.createApplication()).thenReturn(replacement);
    doThrow(primary).when(replacement).commit();
    InputStream upload = new ByteArrayInputStream(Files.readAllBytes(archive));
    String destination = root.toString();

    IOException failure = assertThrows(IOException.class,
        () -> DeploymentInstaller.deploy(upload, "hello-app", destination, admin));

    assertSame(primary, failure.getCause());
    verify(admin).setClasspath(CLASSPATH_NAME, previousClasspath);
    verify(configuration).createApplication();
    try (Stream<Path> directories = Files.list(root.resolve("hello-app"))) {
      assertEquals(List.of(previous), directories.filter(Files::isDirectory).toList());
    }

    assertEquals(previous.getFileName().toString(),
        Files.readString(root.resolve("hello-app/current")));
  }

  @ParameterizedTest
  @MethodSource("getRequiredEntries")
  void shouldRefuseAnArchiveWithoutRequiredEntryAndLeaveNoFiles(String entry, String message)
      throws Exception {
    try (FileSystem zip = FileSystems.newFileSystem(archive)) {
      Files.delete(zip.getPath(entry));
    }

    InputStream upload = new ByteArrayInputStream(Files.readAllBytes(archive));
    String destination = root.toString();

    IOException failure = assertThrows(IOException.class,
        () -> DeploymentInstaller.deploy(upload, "hello-app", destination, admin));

    assertEquals(message, failure.getMessage());
    verify(configuration, never()).createApplication();
    try (Stream<Path> files = Files.list(root)) {
      assertEquals(List.of(root.resolve(".lock"), root.resolve("hello-app")),
          files.sorted().toList());
    }

    try (Stream<Path> directories = Files.list(root.resolve("hello-app"))) {
      assertEquals(List.of(previous), directories.filter(Files::isDirectory).toList());
    }
  }

  @Test
  void shouldRefuseAnArchiveWithoutDependenciesAndLeaveNoFiles() throws Exception {
    try (FileSystem zip = FileSystems.newFileSystem(archive);
        Stream<Path> jars = Files.list(zip.getPath("dependencies"))) {
      for (Path jar : jars.toList()) {
        Files.delete(jar);
      }
    }

    InputStream upload = new ByteArrayInputStream(Files.readAllBytes(archive));
    String destination = root.toString();

    DeploymentException refusal = assertThrows(DeploymentException.class,
        () -> DeploymentInstaller.deploy(upload, "hello-app", destination, admin));

    assertEquals("BBj deployment dependencies folder is missing", refusal.getMessage());

    verify(configuration, never()).createApplication();
    try (Stream<Path> files = Files.list(root)) {
      assertEquals(List.of(root.resolve(".lock"), root.resolve("hello-app")),
          files.sorted().toList());
    }

    try (Stream<Path> directories = Files.list(root.resolve("hello-app"))) {
      assertEquals(List.of(previous), directories.filter(Files::isDirectory).toList());
    }
  }

  private static Stream<Arguments> getMutationFailures() {
    return Stream.of(Operation.values()).flatMap(
        operation -> Stream.of(FailurePoint.values()).map(point -> Arguments.of(operation, point)));
  }

  private static Stream<Arguments> getRequiredEntries() {
    return Stream.of(Arguments.of("bbj/webforj.bbj", "BBj deployment launcher is missing"),
        Arguments.of("application.jar", "BBj deployment application JAR is missing"));
  }

  @Nested
  class DevelopmentRun {

    private static final String CLASS_FOLDER_REFUSAL =
        "BBj development class folder must be an absolute directory on the BBj machine";

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
    void shouldRefuseDevelopmentArchiveWithApplicationJarAndLeaveNoFiles() throws Exception {
      try (FileSystem zip = FileSystems.newFileSystem(development)) {
        Files.copy(app.resolve("target/hello-app-1.0-SNAPSHOT.jar"),
            zip.getPath("application.jar"));
      }

      InputStream upload = new ByteArrayInputStream(Files.readAllBytes(development));
      String destination = root.toString();

      DeploymentException refusal = assertThrows(DeploymentException.class,
          () -> DeploymentInstaller.deploy(upload, "hello-app", destination, admin));

      assertEquals("BBj development archive must not contain application.jar",
          refusal.getMessage());
      verify(configuration, never()).createApplication();
      try (Stream<Path> files = Files.list(root)) {
        assertEquals(List.of(root.resolve(".lock"), root.resolve("hello-app")),
            files.sorted().toList());
      }

      try (Stream<Path> directories = Files.list(root.resolve("hello-app"))) {
        assertEquals(List.of(previous), directories.filter(Files::isDirectory).toList());
      }
    }

    @Test
    void shouldRefuseDevelopmentArchiveWhileDevelopmentRunsAreDisabledAndLeaveNoFiles()
        throws Exception {
      Files.delete(temporary.resolve("development.enabled"));
      InputStream upload = new ByteArrayInputStream(Files.readAllBytes(development));
      String destination = root.toString();

      DeploymentException refusal = assertThrows(DeploymentException.class,
          () -> DeploymentInstaller.deploy(upload, "hello-app", destination, admin));

      assertEquals("Development runs are not enabled on this BBj machine. Run bbjship development "
          + "enable", refusal.getMessage());
      verify(configuration, never()).createApplication();
      try (Stream<Path> files = Files.list(root)) {
        assertEquals(List.of(root.resolve(".lock"), root.resolve("hello-app")),
            files.sorted().toList());
      }

      try (Stream<Path> directories = Files.list(root.resolve("hello-app"))) {
        assertEquals(List.of(previous), directories.filter(Files::isDirectory).toList());
      }
    }

    @ParameterizedTest
    @MethodSource("getInvalidClassFolders")
    void shouldRefuseClassFolderThatIsNotAnAbsoluteDirectoryAndLeaveNoFiles(String properties,
        String message) throws Exception {
      String resources = classFolder.toString();
      try (FileSystem zip = FileSystems.newFileSystem(development)) {
        Files.writeString(zip.getPath("development.properties"),
            properties.replace("<resources>", resources.replace("\\", "\\\\")));
      }

      InputStream upload = new ByteArrayInputStream(Files.readAllBytes(development));
      String destination = root.toString();

      DeploymentException refusal = assertThrows(DeploymentException.class,
          () -> DeploymentInstaller.deploy(upload, "hello-app", destination, admin));

      assertEquals(message.replace("<resources>", resources), refusal.getMessage());
      verify(configuration, never()).createApplication();
      try (Stream<Path> files = Files.list(root)) {
        assertEquals(List.of(root.resolve(".lock"), root.resolve("hello-app")),
            files.sorted().toList());
      }

      try (Stream<Path> directories = Files.list(root.resolve("hello-app"))) {
        assertEquals(List.of(previous), directories.filter(Files::isDirectory).toList());
      }
    }

    @Test
    void shouldRestoreThePreviousDevelopmentRunWhenTheCommitFails() throws Exception {
      Files.writeString(previous.resolve("development"), classFolder.toString());
      BBjAdminException primary = new BBjAdminException("change failed");
      when(configuration.createApplication()).thenReturn(replacement, restored);
      doThrow(primary).doNothing().when(admin).commit();
      InputStream upload = new ByteArrayInputStream(Files.readAllBytes(development));
      String destination = root.toString();

      IOException failure = assertThrows(IOException.class,
          () -> DeploymentInstaller.deploy(upload, "hello-app", destination, admin));

      assertSame(primary, failure.getCause());
      verify(admin).setClasspath(CLASSPATH_NAME, previousClasspath);
      verify(restored).setProperties(previousProperties);
      verify(restored).commit();
      try (Stream<Path> directories = Files.list(root.resolve("hello-app"))) {
        assertEquals(List.of(previous), directories.filter(Files::isDirectory).toList());
      }

      assertEquals(previous.getFileName().toString(),
          Files.readString(root.resolve("hello-app/current")));
      assertEquals(classFolder.toString(), Files.readString(previous.resolve("development")));
    }

    private static Stream<Arguments> getInvalidClassFolders() {
      return Stream.of(Arguments.of("", CLASS_FOLDER_REFUSAL),
          Arguments.of("classFolder=", CLASS_FOLDER_REFUSAL),
          Arguments.of("classFolder=target/classes", CLASS_FOLDER_REFUSAL + ": target/classes"),
          Arguments.of("classFolder=\\u0000", CLASS_FOLDER_REFUSAL + ": \0"),
          Arguments.of("classFolder=<resources>/missing",
              CLASS_FOLDER_REFUSAL + ": <resources>/missing"),
          Arguments.of("classFolder=<resources>/webforj.conf",
              CLASS_FOLDER_REFUSAL + ": <resources>/webforj.conf"),
          Arguments.of("classFolder=<resources>" + File.pathSeparator + "target/classes",
              CLASS_FOLDER_REFUSAL + ": <resources>" + File.pathSeparator + "target/classes"));
    }
  }

  private enum Operation {
    DEPLOY, REMOVE
  }

  private enum FailurePoint {
    TERMINATE, SET_CLASSPATH, COMMIT
  }

  private enum RestorationPoint {
    REMOVE_APPLICATION, SET_CLASSPATH, COMMIT, RESTORE_APPLICATION
  }
}
