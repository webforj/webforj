package com.webforj.plugin.foundation.bbj;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertLinesMatch;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import com.webforj.plugin.foundation.bbj.DeploymentArchive.ArchiveSizeException;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipException;
import java.util.zip.ZipOutputStream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class DeploymentArchiveTest {

  @TempDir
  Path temporary;

  @Test
  void shouldExtractHelloAppArchive() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
    Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
    Path deployment = temporary.resolve("deployment");

    DeploymentArchive.read(archive, deployment);

    try (Stream<Path> files = Files.walk(deployment)) {
      assertLinesMatch(
          List.of("application.jar", "bbj/webforj.bbj",
              "dependencies/00000-webforj-foundation-.+\\.jar",
              "dependencies/00001-webforj-dispatcher-.+\\.jar",
              "dependencies/00002-webforj-data-.+\\.jar"),
          files.filter(Files::isRegularFile)
              .map(file -> deployment.relativize(file).toString().replace('\\', '/')).sorted()
              .toList());
    }

    assertArrayEquals(Files.readAllBytes(app.resolve("target/hello-app-1.0-SNAPSHOT.jar")),
        Files.readAllBytes(deployment.resolve("application.jar")));
  }

  @Test
  void shouldExtractDevelopmentArchiveWithoutApplicationJar() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
    Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
    try (FileSystem zip = FileSystems.newFileSystem(archive)) {
      Files.delete(zip.getPath("application.jar"));
      Files.writeString(zip.getPath("development.properties"), """
          classFolder=/work/hello-app/target/classes
          """);
    }

    Path deployment = temporary.resolve("deployment");

    DeploymentArchive.read(archive, deployment);

    try (Stream<Path> files = Files.walk(deployment)) {
      assertLinesMatch(
          List.of("bbj/webforj.bbj", "dependencies/00000-webforj-foundation-.+\\.jar",
              "dependencies/00001-webforj-dispatcher-.+\\.jar",
              "dependencies/00002-webforj-data-.+\\.jar", "development.properties"),
          files.filter(Files::isRegularFile)
              .map(file -> deployment.relativize(file).toString().replace('\\', '/')).sorted()
              .toList());
    }
  }

  @Test
  void shouldTellDevelopmentArchiveFromApplicationArchive() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
    Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
    Path development = Files.copy(archive, temporary.resolve("development.zip"));
    try (FileSystem zip = FileSystems.newFileSystem(development)) {
      Files.delete(zip.getPath("application.jar"));
      Files.writeString(zip.getPath("development.properties"), """
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
    Path archive = temporary.resolve("hello-app.zip");

    IOException failure =
        assertThrows(IOException.class, () -> DeploymentArchive.write(archive, files));

    assertEquals("Invalid BBj deployment archive entry: " + name, failure.getMessage());
    assertFalse(Files.exists(archive));
  }

  @ParameterizedTest
  @ValueSource(strings = {"../escaped", "dependencies/../../escaped", "dependencies\\escaped",
      "/escaped.bbj", "C:/escaped.bbj"})
  void shouldRefuseUploadedUnsafeEntryAndRemoveTheDeployment(String name) throws IOException {
    Path archive = temporary.resolve("hello-app.zip");
    try (ZipOutputStream zip = new ZipOutputStream(Files.newOutputStream(archive))) {
      zip.putNextEntry(new ZipEntry("application.jar"));
      zip.write("application".getBytes(StandardCharsets.UTF_8));
      zip.putNextEntry(new ZipEntry(name));
      zip.write("escaped".getBytes(StandardCharsets.UTF_8));
    }

    Path deployment = temporary.resolve("deployment");

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
    Path archive = Files.writeString(temporary.resolve("hello-app.zip"), body);
    Path deployment = temporary.resolve("deployment");

    ZipException failure =
        assertThrows(ZipException.class, () -> DeploymentArchive.read(archive, deployment));

    assertEquals("The uploaded body is not a BBj deployment archive. Deploy the ZIP archive the "
        + "build writes", failure.getMessage());
    assertFalse(Files.exists(deployment));
  }

  @Test
  void shouldRefuseUploadThatUnpacksBeyondTheAllowedSizeAndRemoveTheDeployment()
      throws IOException {
    Path archive = temporary.resolve("hello-app.zip");
    try (ZipOutputStream zip = new ZipOutputStream(Files.newOutputStream(archive))) {
      zip.putNextEntry(new ZipEntry("application.jar"));
      zip.write(new byte[4097]);
    }

    Path deployment = temporary.resolve("deployment");
    long maxBytes = 4096;

    ArchiveSizeException failure = assertThrows(ArchiveSizeException.class,
        () -> DeploymentArchive.read(archive, deployment, maxBytes));

    assertEquals("BBj deployment archive exceeds the allowed size of 1 GiB", failure.getMessage());
    assertFalse(Files.exists(deployment));
  }

  @Test
  void shouldKeepExistingDeploymentFolderWhenExtractingIntoIt() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
    Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
    Path deployment = Files.createDirectory(temporary.resolve("deployment"));
    Files.writeString(deployment.resolve("application.jar"), "existing");

    assertThrows(IOException.class, () -> DeploymentArchive.read(archive, deployment));

    assertEquals("existing", Files.readString(deployment.resolve("application.jar")));
  }

  @Test
  void shouldRefuseMissingPayloadAndKeepTheExistingArchive() throws IOException {
    Path archive = Files.writeString(temporary.resolve("hello-app.zip"), "existing");
    Path missing = temporary.resolve("hello-app/target/hello-app-1.0-SNAPSHOT.jar");
    Map<String, Path> files = Map.of("application.jar", missing);

    IOException failure =
        assertThrows(IOException.class, () -> DeploymentArchive.write(archive, files));

    assertEquals("BBj deployment payload file is missing: " + missing, failure.getMessage());
    assertEquals("existing", Files.readString(archive));
  }

  @ParameterizedTest
  @ValueSource(booleans = {false, true})
  void shouldRefuseArchiveThatWouldReplaceTheApplicationJar(boolean hardLink) throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
    Path jar = HelloApp.writeJar(app);

    Path archive = jar;
    if (hardLink) {
      archive = temporary.resolve("hello-app.zip");
      try {
        Files.createLink(archive, jar);
      } catch (UnsupportedOperationException unsupported) {
        assumeTrue(false, "The file system does not support hard links");
      }
    }

    Map<String, Path> files = Map.of("application.jar", jar);
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
    Path archive = temporary.resolve("hello-app.zip");
    try (ZipOutputStream zip = new ZipOutputStream(Files.newOutputStream(archive))) {
      zip.putNextEntry(new ZipEntry("dependencies/"));
    }

    Path deployment = temporary.resolve("deployment");

    DeploymentArchive.read(archive, deployment);

    assertTrue(Files.isDirectory(deployment.resolve("dependencies")));
  }
}
