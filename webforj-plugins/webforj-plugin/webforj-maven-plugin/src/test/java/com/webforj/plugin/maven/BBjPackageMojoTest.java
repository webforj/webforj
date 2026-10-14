package com.webforj.plugin.maven;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import com.webforj.App;
import com.webforj.bbjship.foundation.HelloApp;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import org.apache.maven.artifact.DefaultArtifact;
import org.apache.maven.artifact.handler.DefaultArtifactHandler;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class BBjPackageMojoTest {

  private static final String ARCHIVE_WRITTEN_MESSAGE = "BBj deployment archive written to ";
  private static final String ARCHIVE_DIRECTORY = "target/bbj";

  @TempDir
  Path temporary;

  Path app;
  Path jar;
  Path foundation;
  DefaultArtifact framework;
  Log log;
  BBjPackageMojo mojo;

  @BeforeEach
  void createMojo() throws Exception {
    app = HelloApp.createCopy(temporary);
    jar = HelloApp.writeJar(app);
    foundation = Path.of(App.class.getProtectionDomain().getCodeSource().getLocation().toURI());
    DefaultArtifact application = new DefaultArtifact("org.example", "hello-app", "1.0-SNAPSHOT",
        null, "jar", null, new DefaultArtifactHandler("jar"));
    application.setFile(jar.toFile());
    framework = new DefaultArtifact("com.webforj", "webforj-foundation", "26.03-SNAPSHOT",
        "compile", "jar", null, new DefaultArtifactHandler("jar"));
    framework.setFile(foundation.toFile());
    mojo = new BBjPackageMojo();
    mojo.project = new MavenProject();
    mojo.project.getBuild().setDirectory(app.resolve("target").toString());
    mojo.project.setArtifact(application);
    mojo.project.setArtifacts(Set.of(framework));
    mojo.bbj.setPublishName("hello-app");
    log = mock(Log.class);
    mojo.setLog(log);
  }

  @Test
  void shouldWriteTheArchiveOfTheHelloApp() throws Exception {
    Path archive = app.resolve("target/bbj/hello-app.zip");

    mojo.execute();

    try (ZipFile zip = new ZipFile(archive.toFile())) {
      assertEquals(List.of("dependencies/00000-" + foundation.getFileName(), "application.jar",
          "bbj/webforj.bbj"), zip.stream().map(ZipEntry::getName).toList());
      try (InputStream packed = zip.getInputStream(zip.getEntry("application.jar"))) {
        assertArrayEquals(Files.readAllBytes(jar), packed.readAllBytes());
      }
    }

    verify(log).info(ARCHIVE_WRITTEN_MESSAGE + archive);
  }

  @Test
  void shouldNameTheArchiveAfterTheCommandLinePublishName() throws Exception {
    mojo.publishName = "hello-app-staging";

    mojo.execute();

    verify(log).info(ARCHIVE_WRITTEN_MESSAGE + app.resolve("target/bbj/hello-app-staging.zip"));
  }

  @Test
  void shouldNameTheArchiveAfterTheBuildFinalNameWhenNoPublishNameIsConfigured() throws Exception {
    mojo.bbj.setPublishName("");
    mojo.project.getBuild().setFinalName("hello-app-1.0-SNAPSHOT");

    mojo.execute();

    verify(log)
        .info(ARCHIVE_WRITTEN_MESSAGE + app.resolve("target/bbj/hello-app-1.0-SNAPSHOT.zip"));
  }

  @Test
  void shouldRefuseProjectThatIsNotPackagedAsJar() {
    mojo.project.setPackaging("war");

    MojoExecutionException failure = assertThrows(MojoExecutionException.class, mojo::execute);

    assertEquals("BBj deployment requires jar packaging, this project is packaged as war",
        failure.getMessage());
    assertFalse(Files.exists(app.resolve(ARCHIVE_DIRECTORY)));
  }

  @Test
  void shouldRefuseToPackageWhenTheArtifactHasNoFile() {
    mojo.project.getArtifact().setFile(null);

    MojoExecutionException failure = assertThrows(MojoExecutionException.class, mojo::execute);

    assertEquals(
        "BBj deployment requires the packaged application jar. Run the package phase "
            + "in the same invocation, as in mvn package webforj:bbj-package",
        failure.getMessage());
    assertFalse(Files.exists(app.resolve(ARCHIVE_DIRECTORY)));
  }

  @Test
  void shouldRefuseToPackageWithoutPublishName() {
    mojo.bbj.setPublishName("");

    MojoExecutionException failure = assertThrows(MojoExecutionException.class, mojo::execute);

    assertEquals("BBj deployment requires bbj.publishName or a final name of the build",
        failure.getMessage());
    assertFalse(Files.exists(app.resolve(ARCHIVE_DIRECTORY)));
  }

  @ParameterizedTest
  @MethodSource("getRefusedPublishNames")
  void shouldRefuseInvalidPublishNameBeforeWritingTheArchive(String publishName, String message) {
    mojo.bbj.setPublishName(publishName);

    MojoExecutionException failure = assertThrows(MojoExecutionException.class, mojo::execute);

    assertEquals(message, failure.getMessage());
    assertFalse(Files.exists(app.resolve(ARCHIVE_DIRECTORY)));
    assertFalse(Files.exists(app.resolve("target/escaped.zip")));
  }

  @Test
  void shouldReportTheArchiveThatCannotBeWritten() {
    framework.setFile(null);

    MojoExecutionException failure = assertThrows(MojoExecutionException.class, mojo::execute);

    assertEquals("BBj deployment archive could not be written. Check the application package "
        + "and dependencies", failure.getMessage());
    assertEquals("BBj deployment dependency is unresolved. Resolve the runtime classpath",
        failure.getCause().getMessage());
    assertFalse(Files.exists(app.resolve("target/bbj/hello-app.zip")));
  }

  private static Stream<Arguments> getRefusedPublishNames() {
    return Stream.of(Arguments.of("../escaped",
        "Publish name ../escaped is invalid. Use a name that starts with a letter "
            + "or digit, holds only letters, digits, underscores, dots and hyphens, and is not a "
            + "reserved device name such as con or nul"),
        Arguments.of("hello/app",
            "Publish name hello/app is invalid. Use a name that starts with a letter "
                + "or digit, holds only letters, digits, underscores, dots and hyphens, and is "
                + "not a reserved device name such as con or nul"),
        Arguments.of("hello.",
            "Publish name hello. is invalid. Use a name that starts with a letter "
                + "or digit, holds only letters, digits, underscores, dots and hyphens, and is "
                + "not a reserved device name such as con or nul"),
        Arguments.of("webapp", "The publish name webapp is reserved by BBjServices for its own "
            + "web paths. Choose another publish name"));
  }
}
