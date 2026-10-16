package com.webforj.bbjship.foundation;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertLinesMatch;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import com.webforj.bbjship.foundation.DeploymentArchive.ArchiveSizeException;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Stream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipException;
import java.util.zip.ZipOutputStream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;

class DeploymentArchiveTest {

  private static final String APP_NAME = "hello-app";
  private static final String ARCHIVE_FILE = "hello-app.zip";
  private static final String DEPLOYMENT_FOLDER = "deployment";
  private static final String APPLICATION_JAR = "application.jar";
  private static final String DEVELOPMENT_PROPERTIES = "development.properties";
  private static final String EXISTING_CONTENT = "existing";

  @TempDir
  Path temporary;

  @Test
  void shouldExtractHelloAppArchive() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_FILE));
    Path deployment = temporary.resolve(DEPLOYMENT_FOLDER);

    DeploymentArchive.read(archive, deployment);

    try (Stream<Path> files = Files.walk(deployment)) {
      assertLinesMatch(
          List.of(APPLICATION_JAR, "bbj/webforj.bbj",
              "dependencies/00000-webforj-foundation-.+\\.jar",
              "dependencies/00001-webforj-dispatcher-.+\\.jar",
              "dependencies/00002-webforj-data-.+\\.jar"),
          files.filter(Files::isRegularFile)
              .map(file -> deployment.relativize(file).toString().replace('\\', '/')).sorted()
              .toList());
    }

    assertArrayEquals(Files.readAllBytes(app.resolve("target/hello-app-1.0-SNAPSHOT.jar")),
        Files.readAllBytes(deployment.resolve(APPLICATION_JAR)));
  }

  @Test
  void shouldExtractDevelopmentArchiveWithoutApplicationJar() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_FILE));
    try (FileSystem zip = FileSystems.newFileSystem(archive)) {
      Files.delete(zip.getPath(APPLICATION_JAR));
      Files.writeString(zip.getPath(DEVELOPMENT_PROPERTIES), """
          classFolder=/work/hello-app/target/classes
          """);
    }

    Path deployment = temporary.resolve(DEPLOYMENT_FOLDER);

    DeploymentArchive.read(archive, deployment);

    try (Stream<Path> files = Files.walk(deployment)) {
      assertLinesMatch(
          List.of("bbj/webforj.bbj", "dependencies/00000-webforj-foundation-.+\\.jar",
              "dependencies/00001-webforj-dispatcher-.+\\.jar",
              "dependencies/00002-webforj-data-.+\\.jar", DEVELOPMENT_PROPERTIES),
          files.filter(Files::isRegularFile)
              .map(file -> deployment.relativize(file).toString().replace('\\', '/')).sorted()
              .toList());
    }
  }

  @Test
  void shouldTellDevelopmentArchiveFromApplicationArchive() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_FILE));
    Path development = Files.copy(archive, temporary.resolve("development.zip"));
    try (FileSystem zip = FileSystems.newFileSystem(development)) {
      Files.delete(zip.getPath(APPLICATION_JAR));
      Files.writeString(zip.getPath(DEVELOPMENT_PROPERTIES), """
          classFolder=/work/hello-app/target/classes
          """);
    }

    assertFalse(DeploymentArchive.isDevelopment(archive));
    assertTrue(DeploymentArchive.isDevelopment(development));
  }

  @ParameterizedTest
  @ValueSource(strings = {"../escaped", "dependencies/../../escaped", "dependencies\\escaped",
      "/escaped.bbj", "C:/escaped.bbj", "description.properties", "bbj/readme.txt"})
  void shouldRefuseUnsafeEntryNameBeforeWriting(String name) throws IOException {
    Map<String, Path> files = Map.of(name, HelloApp.getFile("webforj.conf"));
    Path archive = temporary.resolve(ARCHIVE_FILE);

    IOException failure =
        assertThrows(IOException.class, () -> DeploymentArchive.write(archive, files));

    assertEquals("Invalid BBj deployment archive entry: " + name, failure.getMessage());
    assertFalse(Files.exists(archive));
  }

  @ParameterizedTest
  @ValueSource(strings = {"../escaped", "dependencies/../../escaped", "dependencies\\escaped",
      "/escaped.bbj", "C:/escaped.bbj"})
  void shouldRefuseUploadedUnsafeEntryAndRemoveTheDeployment(String name) throws IOException {
    Path archive = temporary.resolve(ARCHIVE_FILE);
    try (ZipOutputStream zip = new ZipOutputStream(Files.newOutputStream(archive))) {
      zip.putNextEntry(new ZipEntry(APPLICATION_JAR));
      zip.write("application".getBytes(StandardCharsets.UTF_8));
      zip.putNextEntry(new ZipEntry(name));
      zip.write("escaped".getBytes(StandardCharsets.UTF_8));
    }

    Path deployment = temporary.resolve(DEPLOYMENT_FOLDER);

    ZipException failure =
        assertThrows(ZipException.class, () -> DeploymentArchive.read(archive, deployment));

    assertEquals("Invalid BBj deployment archive entry: " + name, failure.getMessage());
    assertFalse(Files.exists(deployment));
    assertFalse(Files.exists(temporary.resolve("escaped")));
  }

  @ParameterizedTest
  @ValueSource(strings = {"not a deployment archive", ""})
  void shouldRefuseUploadThatIsNotZipAtTheFirstEntryAndRemoveTheDeployment(String body)
      throws IOException {
    Path archive = Files.writeString(temporary.resolve(ARCHIVE_FILE), body);
    Path deployment = temporary.resolve(DEPLOYMENT_FOLDER);

    ZipException failure =
        assertThrows(ZipException.class, () -> DeploymentArchive.read(archive, deployment));

    assertEquals("The uploaded body is not a BBj deployment archive. Deploy the ZIP archive the "
        + "build writes", failure.getMessage());
    assertFalse(Files.exists(deployment));
  }

  @Test
  void shouldRefuseUploadThatUnpacksBeyondTheAllowedSizeAndRemoveTheDeployment()
      throws IOException {
    Path archive = temporary.resolve(ARCHIVE_FILE);
    try (ZipOutputStream zip = new ZipOutputStream(Files.newOutputStream(archive))) {
      zip.putNextEntry(new ZipEntry(APPLICATION_JAR));
      zip.write(new byte[4097]);
    }

    Path deployment = temporary.resolve(DEPLOYMENT_FOLDER);
    long maxBytes = 4096;

    ArchiveSizeException failure = assertThrows(ArchiveSizeException.class,
        () -> DeploymentArchive.read(archive, deployment, maxBytes));

    assertEquals("BBj deployment archive exceeds the allowed size of 1 GiB", failure.getMessage());
    assertFalse(Files.exists(deployment));
  }

  @Test
  void shouldKeepExistingDeploymentFolderWhenExtractingIntoIt() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_FILE));
    Path deployment = Files.createDirectory(temporary.resolve(DEPLOYMENT_FOLDER));
    Files.writeString(deployment.resolve(APPLICATION_JAR), EXISTING_CONTENT);

    assertThrows(IOException.class, () -> DeploymentArchive.read(archive, deployment));

    assertEquals(EXISTING_CONTENT, Files.readString(deployment.resolve(APPLICATION_JAR)));
  }

  @Test
  void shouldRefuseMissingPayloadAndKeepTheExistingArchive() throws IOException {
    Path archive = Files.writeString(temporary.resolve(ARCHIVE_FILE), EXISTING_CONTENT);
    Path missing = temporary.resolve("hello-app/target/hello-app-1.0-SNAPSHOT.jar");
    Map<String, Path> files = Map.of(APPLICATION_JAR, missing);

    IOException failure =
        assertThrows(IOException.class, () -> DeploymentArchive.write(archive, files));

    assertEquals("BBj deployment payload file is missing: " + missing, failure.getMessage());
    assertEquals(EXISTING_CONTENT, Files.readString(archive));
  }

  @ParameterizedTest
  @ValueSource(booleans = {false, true})
  void shouldRefuseArchiveThatWouldReplaceTheApplicationJar(boolean hardLink) throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Path jar = HelloApp.writeJar(app);

    Path archive = jar;
    if (hardLink) {
      archive = temporary.resolve(ARCHIVE_FILE);
      try {
        Files.createLink(archive, jar);
      } catch (UnsupportedOperationException unsupported) {
        assumeTrue(false, "The file system does not support hard links");
      }
    }

    Map<String, Path> files = Map.of(APPLICATION_JAR, jar);
    byte[] content = Files.readAllBytes(jar);
    Path output = archive;

    IOException failure =
        assertThrows(IOException.class, () -> DeploymentArchive.write(output, files));

    assertEquals(
        "BBj deployment archive must not replace a payload source. Choose another output path",
        failure.getMessage());
    assertArrayEquals(content, Files.readAllBytes(jar));
  }

  @Test
  void shouldExtractUploadedDirectoryEntries() throws IOException {
    Path archive = temporary.resolve(ARCHIVE_FILE);
    try (ZipOutputStream zip = new ZipOutputStream(Files.newOutputStream(archive))) {
      zip.putNextEntry(new ZipEntry("dependencies/"));
    }

    Path deployment = temporary.resolve(DEPLOYMENT_FOLDER);

    DeploymentArchive.read(archive, deployment);

    assertTrue(Files.isDirectory(deployment.resolve("dependencies")));
  }

  @Test
  void shouldSaveTheUploadedArchive() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_FILE));
    Path saved = temporary.resolve("upload.zip");

    try (InputStream upload = Files.newInputStream(archive)) {
      DeploymentArchive.save(upload, saved);
    }

    assertArrayEquals(Files.readAllBytes(archive), Files.readAllBytes(saved));
  }

  @Test
  void shouldRefuseUploadBeyondTheAllowedSize() {
    InputStream upload = new ByteArrayInputStream(new byte[4097]);
    Path saved = temporary.resolve("upload.zip");
    long maxBytes = 4096;

    ArchiveSizeException failure = assertThrows(ArchiveSizeException.class,
        () -> DeploymentArchive.save(upload, saved, maxBytes));

    assertEquals("BBj deployment archive exceeds the allowed size of 1 GiB", failure.getMessage());
  }

  @Test
  void shouldFindTheClassFoldersOfDevelopmentArchiveInOrder() throws IOException {
    Path deployment = Files.createDirectory(temporary.resolve(DEPLOYMENT_FOLDER));
    Path classes = Path.of("/work/hello-app/target/classes");
    Path resources = Path.of("/work/hello-app/target/resources");
    DeploymentArchive.writeClassFolders(deployment.resolve(DEVELOPMENT_PROPERTIES),
        List.of(classes, resources));

    Optional<List<String>> classFolders = DeploymentArchive.findClassFolders(deployment);

    assertEquals(Optional.of(List.of(classes.toString(), resources.toString())), classFolders);
  }

  @Test
  void shouldFindNoClassFoldersOfApplicationArchive() throws IOException {
    Path deployment = Files.createDirectory(temporary.resolve(DEPLOYMENT_FOLDER));

    assertEquals(Optional.empty(), DeploymentArchive.findClassFolders(deployment));
  }

  @Test
  void shouldFindEmptyClassFoldersWhenTheDevelopmentPropertiesNameNone() throws IOException {
    Path deployment = Files.createDirectory(temporary.resolve(DEPLOYMENT_FOLDER));
    Files.writeString(deployment.resolve(DEVELOPMENT_PROPERTIES), "classFolder= \n");

    assertEquals(Optional.of(List.of()), DeploymentArchive.findClassFolders(deployment));
  }

  @Test
  void shouldNameTheArchiveAfterThePublishNameInTheBuildDirectory() {
    Path archive = DeploymentArchive.resolveArchive(Path.of("/work/hello-app/build"), APP_NAME);

    assertEquals(Path.of("/work/hello-app/build/bbj/hello-app.zip"), archive);
  }

  @ParameterizedTest
  @MethodSource("getRefusedPublishNames")
  void shouldRefuseThePublishNameOfTheArchive(String publishName, String message) {
    Path buildDirectory = Path.of("/work/hello-app/build");

    IllegalArgumentException failure = assertThrows(IllegalArgumentException.class,
        () -> DeploymentArchive.resolveArchive(buildDirectory, publishName));

    assertEquals(message, failure.getMessage());
  }

  private static Stream<Arguments> getRefusedPublishNames() {
    return Stream.of(
        Arguments.of("", "BBj deployment requires bbj.publishName or a final name of the build"),
        Arguments.of("../escaped",
            "Publish name ../escaped is invalid. Use a name that starts with a letter or digit, "
                + "holds only letters, digits, underscores, dots and hyphens, and is not a "
                + "reserved device name such as con or nul"),
        Arguments.of("webapp", "The publish name webapp is reserved by BBjServices for its own "
            + "web paths. Choose another publish name"));
  }
}
