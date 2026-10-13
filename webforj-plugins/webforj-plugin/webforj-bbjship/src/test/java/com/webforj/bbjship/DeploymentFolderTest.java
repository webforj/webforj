package com.webforj.bbjship;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import com.webforj.plugin.foundation.bbj.DeploymentArchive;
import com.webforj.plugin.foundation.bbj.HelloApp;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class DeploymentFolderTest {

  private static final String APP_NAME = "hello-app";
  private static final String CURRENT_FILE = "hello-app/current";
  private static final String APPLICATION_JAR = "application.jar";

  @TempDir
  Path temporary;

  private Path root;
  private Path previous;

  @BeforeEach
  void initHelloAppDeployment() throws Exception {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
    root = temporary.resolve("deployments");
    previous =
        Files.createDirectories(root.resolve(APP_NAME)).resolve(UUID.randomUUID().toString());
    DeploymentArchive.read(archive, previous);
  }

  @Test
  void shouldFindTheDirectoryTheCurrentFileNames() throws IOException {
    Files.writeString(root.resolve(CURRENT_FILE), previous.getFileName() + "\n");

    Optional<Path> current = DeploymentFolder.findCurrentDirectory(root, APP_NAME);

    assertEquals(Optional.of(previous), current);
  }

  @ParameterizedTest
  @MethodSource("provideMissingCurrentDirectories")
  void shouldFindNoCurrentDirectory(String name, String current) throws IOException {
    if (current != null) {
      Files.writeString(root.resolve(CURRENT_FILE), current);
    }

    assertEquals(Optional.empty(), DeploymentFolder.findCurrentDirectory(root, name));
  }

  @Test
  void shouldReadClassFoldersAsApplicationSources() throws Exception {
    Path classes = temporary.resolve("classes");
    Path resources = temporary.resolve("resources");
    DevelopmentRunFiles.write(previous, classes, resources);

    List<Path> sources = DeploymentFolder.readApplicationSources(previous);

    assertEquals(List.of(classes, resources), sources);
  }

  @Test
  void shouldReadApplicationJarAsApplicationSources() throws Exception {
    List<Path> sources = DeploymentFolder.readApplicationSources(previous);

    assertEquals(List.of(previous.resolve(APPLICATION_JAR)), sources);
  }

  @Test
  void shouldReadNoApplicationSourcesInEmptyDirectory() throws Exception {
    Path empty = Files.createDirectories(temporary.resolve("empty"));

    List<Path> sources = DeploymentFolder.readApplicationSources(empty);

    assertTrue(sources.isEmpty());
  }

  @Test
  void shouldReadTheClassFoldersTheDevelopmentPropertiesName() throws IOException {
    List<Path> classFolders = List.of(temporary.resolve("classes"), temporary.resolve("resources"));
    DevelopmentRunFiles.write(previous, classFolders.toArray(Path[]::new));

    assertEquals(classFolders, DeploymentFolder.readClassFolders(previous));
  }

  @Test
  void shouldResolveOnlyTheJarsDirectlyInsideTheDependenciesFolderInFileNameOrder()
      throws IOException {
    Path jar = previous.resolve(APPLICATION_JAR);
    Path dependencies = Files.createDirectories(temporary.resolve("deployment/dependencies"));
    Files.copy(jar, dependencies.resolve("00001-hello-app.jar"));
    Files.copy(jar, dependencies.resolve("00000-hello-app.jar"));
    Files.copy(jar, dependencies.resolve("00009-hello-app.zip"));
    Files.createDirectories(dependencies.resolve("nested"));
    Files.copy(jar, dependencies.resolve("nested/00009-hello-app.jar"));

    List<Path> jars = DeploymentFolder.resolveDependencyJars(temporary.resolve("deployment"));

    assertEquals(List.of(dependencies.resolve("00000-hello-app.jar"),
        dependencies.resolve("00001-hello-app.jar")), jars);
  }

  @Test
  void shouldWriteTheCurrentFileOfTheDeployment() throws IOException {
    DeploymentFolder.writeCurrent(previous);

    assertEquals(previous.getFileName().toString(), Files.readString(root.resolve(CURRENT_FILE)));
  }

  @Test
  void shouldRemoveTheDeploymentWithItsCurrentFileAndEmptyFolder() throws IOException {
    DeploymentFolder.writeCurrent(previous);

    Optional<String> kept = DeploymentFolder.removeDeployment(previous);

    assertEquals(Optional.empty(), kept);
    assertFalse(Files.exists(root.resolve(APP_NAME)));
  }

  @Test
  void shouldReportTheFolderWhoseFilesCannotBeRemoved() {
    assumeTrue(root.getFileSystem().supportedFileAttributeViews().contains("posix"),
        "The removal failure needs a posix file system");
    Path folder = root.resolve(APP_NAME);
    assertTrue(previous.toFile().setWritable(false));

    Optional<String> kept;
    try {
      kept = DeploymentFolder.removeDeployment(previous);
    } finally {
      assertTrue(previous.toFile().setWritable(true));
    }

    assertTrue(kept.orElseThrow().startsWith("Not removed: " + folder + ". "));
    assertTrue(Files.isRegularFile(previous.resolve(APPLICATION_JAR)));
  }

  @Test
  void shouldRemoveTheLeftoverDeploymentsBesideTheCurrentOne() throws IOException {
    Path current = Files.createDirectories(root.resolve("hello-app/" + UUID.randomUUID()));
    Path other = Files.createDirectories(root.resolve("hello-app/other"));

    DeploymentFolder.removeLeftoverDirectories(current);

    assertTrue(Files.isDirectory(other));
    assertTrue(Files.isDirectory(current));
    assertFalse(Files.exists(previous));
  }

  @Test
  void shouldFormatTheReportOfTheKeptFolder() {
    String report = DeploymentFolder.formatKeptFolder(previous, new IOException("held"));

    assertEquals("Not removed: " + previous + ". held", report);
  }

  private static Stream<Arguments> provideMissingCurrentDirectories() {
    return Stream.of(Arguments.of("Hello App", null), Arguments.of(APP_NAME, null),
        Arguments.of(APP_NAME, "not-an-id"), Arguments.of(APP_NAME, UUID.randomUUID().toString()));
  }
}
