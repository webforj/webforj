package com.webforj.bbjship.deployer;

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
import com.webforj.bbjship.ClasspathRelease;
import com.webforj.bbjship.Deployer;
import com.webforj.bbjship.DeploymentException;
import com.webforj.bbjship.DevelopmentRunFiles;
import com.webforj.bbjship.foundation.DeploymentArchive;
import com.webforj.bbjship.foundation.HelloApp;
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
import java.util.Optional;
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

final class DeployerFailureTest {

  private static final String CLASSPATH_NAME = "webforj_bbjship_hello-app";
  private static final String NAME = "hello-app";
  private static final String CURRENT_FILE = "hello-app/current";
  private static final String APPLICATION_JAR = "application.jar";
  private static final String PREVIOUS_ARGUMENT = "--previous";
  private static final String PREVIOUS_JAVA_ARGUMENT = "-Dprevious=true";
  private static final String PUBLICATION_FAILURE = "publication failed";
  private static final String LOCK_FILE = ".lock";

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
    app = HelloApp.createCopy(temporary.resolve(NAME));
    archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
    root = temporary.resolve("deployments");
    previous = Files.createDirectories(root.resolve(NAME)).resolve(UUID.randomUUID().toString());
    DeploymentArchive.read(archive, previous);
    Files.writeString(root.resolve(CURRENT_FILE), previous.getFileName().toString());
    String program = previous.resolve("bbj/webforj.bbj").toString();
    previousClasspath = List.of("(_webforj_bbjship)",
        previous.resolve("dependencies/00000-webforj-plugin-foundation-26.03-SNAPSHOT.jar")
            .toString(),
        previous.resolve("dependencies/00001-webforj-foundation-26.03-SNAPSHOT.jar").toString(),
        previous.resolve("dependencies/00002-webforj-dispatcher-26.03-SNAPSHOT.jar").toString(),
        previous.resolve("dependencies/00003-webforj-data-26.03-SNAPSHOT.jar").toString(),
        previous.resolve(APPLICATION_JAR).toString());
    previousProperties = Map.of(BBjAdminAppDeploymentApplication.NAME, NAME,
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
    when(registered.getString(BBjAdminAppDeploymentApplication.NAME)).thenReturn(NAME);
    when(registered.getString(BBjAdminAppDeploymentApplication.PROGRAM)).thenReturn(program);
    when(registered.getString(BBjAdminAppDeploymentApplication.CLASSPATH))
        .thenReturn(CLASSPATH_NAME);
    when(registered.getProperties()).thenReturn(previousProperties);
    when(registered.getArguments())
        .thenReturn(new BBjAdminAdapterList<>(List.of(PREVIOUS_ARGUMENT)));
    when(registered.getJavaArguments())
        .thenReturn(new BBjAdminAdapterList<>(List.of(PREVIOUS_JAVA_ARGUMENT)));
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
    when(admin.getBBjProcesses()).thenReturn(List.of(owned, foreign), List.of());
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
            ? () -> Deployer.deploy(upload, NAME, destination, admin, ClasspathRelease.NONE)
            : () -> Deployer.undeploy(NAME, destination, admin, ClasspathRelease.NONE));

    assertSame(primary, failure.getCause());
    assertEquals(0, failure.getSuppressed().length);
    verify(admin).setClasspath(CLASSPATH_NAME, previousClasspath);
    verify(restored).setProperties(previousProperties);
    assertEquals(List.of(PREVIOUS_ARGUMENT), restored.getArguments());
    assertEquals(List.of(PREVIOUS_JAVA_ARGUMENT), restored.getJavaArguments());
    verify(restored).commit();
    verify(foreign, never()).terminate();
    assertArrayEquals(Files.readAllBytes(app.resolve("target/hello-app-1.0-SNAPSHOT.jar")),
        Files.readAllBytes(previous.resolve(APPLICATION_JAR)));
    try (Stream<Path> directories = Files.list(root.resolve(NAME))) {
      assertEquals(List.of(previous), directories.filter(Files::isDirectory).toList());
    }

    assertEquals(previous.getFileName().toString(), Files.readString(root.resolve(CURRENT_FILE)));
  }

  @ParameterizedTest
  @EnumSource(RestorationPoint.class)
  void shouldFinishTheRestorationAndKeepEachOfItsFailures(RestorationPoint point) throws Exception {
    BBjAdminException primary = new BBjAdminException(PUBLICATION_FAILURE);
    BBjAdminException cleanup = new BBjAdminException("restoration failed");
    when(configuration.createApplication()).thenReturn(replacement, restored);
    doThrow(primary).when(replacement).commit();
    switch (point) {
      case REMOVE_APPLICATION ->
        doNothing().doThrow(cleanup).when(configuration).removeApplication(NAME);
      case SET_CLASSPATH ->
        doNothing().doThrow(cleanup).when(admin).setClasspath(anyString(), any());
      case COMMIT -> doNothing().doThrow(cleanup).when(admin).commit();
      case RESTORE_APPLICATION -> doThrow(cleanup).when(restored).commit();
      default -> throw new IllegalArgumentException(point.name());
    }

    InputStream upload = new ByteArrayInputStream(Files.readAllBytes(archive));
    String destination = root.toString();

    IOException failure = assertThrows(IOException.class,
        () -> Deployer.deploy(upload, NAME, destination, admin, ClasspathRelease.NONE));

    assertSame(primary, failure.getCause());
    assertEquals(List.of(cleanup), List.of(failure.getSuppressed()));
    verify(admin).setClasspath(CLASSPATH_NAME, previousClasspath);
    verify(restored).setProperties(previousProperties);
    assertEquals(List.of(PREVIOUS_ARGUMENT), restored.getArguments());
    assertEquals(List.of(PREVIOUS_JAVA_ARGUMENT), restored.getJavaArguments());
    verify(restored).commit();
    try (Stream<Path> directories = Files.list(root.resolve(NAME))) {
      assertEquals(List.of(previous), directories.filter(Files::isDirectory).toList());
    }

    assertEquals(previous.getFileName().toString(), Files.readString(root.resolve(CURRENT_FILE)));
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
            ? () -> Deployer.deploy(upload, NAME, destination, admin, ClasspathRelease.NONE)
            : () -> Deployer.undeploy(NAME, destination, admin, ClasspathRelease.NONE));

    assertSame(primary, failure.getCause());
    verify(configuration, never()).createApplication();
    verify(configuration, never()).removeApplication(anyString());
    verify(admin, never()).setClasspath(anyString(), any());
    verify(admin, never()).commit();
    verify(admin, never()).getBBjProcesses();
    try (Stream<Path> directories = Files.list(root.resolve(NAME))) {
      assertEquals(List.of(previous), directories.filter(Files::isDirectory).toList());
    }

    assertEquals(previous.getFileName().toString(), Files.readString(root.resolve(CURRENT_FILE)));
  }

  @Test
  void shouldRemoveTheNewFolderAndRegistrationWhenTheFirstPublicationFails() throws Exception {
    BBjAdminException primary = new BBjAdminException(PUBLICATION_FAILURE);
    when(configuration.getApplications()).thenReturn(new BBjAdminAdapterList<>(List.of()));
    when(admin.getClasspathNames()).thenReturn(new BBjAdminAdapterList<>(List.of()));
    when(configuration.createApplication()).thenReturn(replacement);
    doThrow(primary).when(replacement).commit();
    Path first = temporary.resolve("first-deployments");
    InputStream upload = new ByteArrayInputStream(Files.readAllBytes(archive));
    String destination = first.toString();

    IOException failure = assertThrows(IOException.class,
        () -> Deployer.deploy(upload, NAME, destination, admin, ClasspathRelease.NONE));

    assertEquals("BBj deployment failed for hello-app. Check deployment settings and the "
        + "BBjServices log", failure.getMessage());
    assertSame(primary, failure.getCause());
    verify(configuration).removeApplication(NAME);
    verify(admin).setClasspath(CLASSPATH_NAME, null);
    try (Stream<Path> directories = Files.list(first)) {
      assertEquals(List.of(first.resolve(LOCK_FILE)), directories.toList());
    }
  }

  @Test
  void shouldRestoreTheLeftoverClasspathWhenItsClaimFails() throws Exception {
    BBjAdminException primary = new BBjAdminException(PUBLICATION_FAILURE);
    when(configuration.getApplications()).thenReturn(new BBjAdminAdapterList<>(List.of()));
    when(configuration.createApplication()).thenReturn(replacement);
    doThrow(primary).when(replacement).commit();
    InputStream upload = new ByteArrayInputStream(Files.readAllBytes(archive));
    String destination = root.toString();

    IOException failure = assertThrows(IOException.class,
        () -> Deployer.deploy(upload, NAME, destination, admin, ClasspathRelease.NONE));

    assertSame(primary, failure.getCause());
    verify(admin).setClasspath(CLASSPATH_NAME, previousClasspath);
    verify(configuration).createApplication();
    try (Stream<Path> directories = Files.list(root.resolve(NAME))) {
      assertEquals(List.of(previous), directories.filter(Files::isDirectory).toList());
    }

    assertEquals(previous.getFileName().toString(), Files.readString(root.resolve(CURRENT_FILE)));
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
        () -> Deployer.deploy(upload, NAME, destination, admin, ClasspathRelease.NONE));

    assertEquals(message, failure.getMessage());
    verify(configuration, never()).createApplication();
    try (Stream<Path> files = Files.list(root)) {
      assertEquals(List.of(root.resolve(LOCK_FILE), root.resolve(NAME)), files.sorted().toList());
    }

    try (Stream<Path> directories = Files.list(root.resolve(NAME))) {
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
        () -> Deployer.deploy(upload, NAME, destination, admin, ClasspathRelease.NONE));

    assertEquals("BBj deployment dependencies folder is missing", refusal.getMessage());

    verify(configuration, never()).createApplication();
    try (Stream<Path> files = Files.list(root)) {
      assertEquals(List.of(root.resolve(LOCK_FILE), root.resolve(NAME)), files.sorted().toList());
    }

    try (Stream<Path> directories = Files.list(root.resolve(NAME))) {
      assertEquals(List.of(previous), directories.filter(Files::isDirectory).toList());
    }
  }

  private static Stream<Arguments> getMutationFailures() {
    return Stream.of(Operation.values()).flatMap(
        operation -> Stream.of(FailurePoint.values()).map(point -> Arguments.of(operation, point)));
  }

  private static Stream<Arguments> getRequiredEntries() {
    return Stream.of(Arguments.of("bbj/webforj.bbj", "BBj deployment launcher is missing"),
        Arguments.of(APPLICATION_JAR, "BBj deployment application JAR is missing"));
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
        Files.delete(zip.getPath(APPLICATION_JAR));
        Files.writeString(zip.getPath("development.properties"), """
            classFolder=%s
            """.formatted(classFolder.toString().replace("\\", "\\\\")));
      }

      Files.createFile(temporary.resolve("development.enabled"));
    }

    @Test
    void shouldRefuseDevelopmentArchiveWithApplicationJarAndLeaveNoFiles() throws Exception {
      try (FileSystem zip = FileSystems.newFileSystem(development)) {
        Files.copy(app.resolve("target/hello-app-1.0-SNAPSHOT.jar"), zip.getPath(APPLICATION_JAR));
      }

      InputStream upload = new ByteArrayInputStream(Files.readAllBytes(development));
      String destination = root.toString();

      DeploymentException refusal = assertThrows(DeploymentException.class,
          () -> Deployer.deploy(upload, NAME, destination, admin, ClasspathRelease.NONE));

      assertEquals("BBj development archive must not contain application.jar",
          refusal.getMessage());
      verify(configuration, never()).createApplication();
      try (Stream<Path> files = Files.list(root)) {
        assertEquals(List.of(root.resolve(LOCK_FILE), root.resolve(NAME)), files.sorted().toList());
      }

      try (Stream<Path> directories = Files.list(root.resolve(NAME))) {
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
          () -> Deployer.deploy(upload, NAME, destination, admin, ClasspathRelease.NONE));

      assertEquals("Development runs are not enabled on this BBj machine. Run bbjship development "
          + "enable", refusal.getMessage());
      verify(configuration, never()).createApplication();
      try (Stream<Path> files = Files.list(root)) {
        assertEquals(List.of(root.resolve(LOCK_FILE), root.resolve(NAME)), files.sorted().toList());
      }

      try (Stream<Path> directories = Files.list(root.resolve(NAME))) {
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
          () -> Deployer.deploy(upload, NAME, destination, admin, ClasspathRelease.NONE));

      assertEquals(message.replace("<resources>", resources), refusal.getMessage());
      verify(configuration, never()).createApplication();
      try (Stream<Path> files = Files.list(root)) {
        assertEquals(List.of(root.resolve(LOCK_FILE), root.resolve(NAME)), files.sorted().toList());
      }

      try (Stream<Path> directories = Files.list(root.resolve(NAME))) {
        assertEquals(List.of(previous), directories.filter(Files::isDirectory).toList());
      }
    }

    @Test
    void shouldRestoreThePreviousDevelopmentRunWhenTheCommitFails() throws Exception {
      DevelopmentRunFiles.write(previous, classFolder);
      BBjAdminException primary = new BBjAdminException("change failed");
      when(configuration.createApplication()).thenReturn(replacement, restored);
      doThrow(primary).doNothing().when(admin).commit();
      InputStream upload = new ByteArrayInputStream(Files.readAllBytes(development));
      String destination = root.toString();

      IOException failure = assertThrows(IOException.class,
          () -> Deployer.deploy(upload, NAME, destination, admin, ClasspathRelease.NONE));

      assertSame(primary, failure.getCause());
      verify(admin).setClasspath(CLASSPATH_NAME, previousClasspath);
      verify(restored).setProperties(previousProperties);
      verify(restored).commit();
      try (Stream<Path> directories = Files.list(root.resolve(NAME))) {
        assertEquals(List.of(previous), directories.filter(Files::isDirectory).toList());
      }

      assertEquals(previous.getFileName().toString(), Files.readString(root.resolve(CURRENT_FILE)));
      assertEquals(Optional.of(List.of(classFolder.toString())),
          DeploymentArchive.findClassFolders(previous));
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
