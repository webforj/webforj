package com.webforj.plugin.foundation.bbj;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertLinesMatch;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.webforj.App;
import com.webforj.plugin.foundation.resolve.ApplicationClasspath;
import com.webforj.plugin.foundation.resolve.ApplicationClasspath.ResolvedJar;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class DeploymentTest {

  @TempDir
  Path temporary;

  @Test
  void shouldPackHelloAppWithWebforjJarsAndLauncherWithoutCredentials() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
    Path archive = temporary.resolve("hello-app.zip");

    Path written = HelloApp.writeArchive(app, archive);

    assertEquals(archive, written);
    try (ZipFile zip = new ZipFile(archive.toFile())) {
      assertLinesMatch(
          List.of("dependencies/00000-webforj-foundation-.+\\.jar",
              "dependencies/00001-webforj-dispatcher-.+\\.jar",
              "dependencies/00002-webforj-data-.+\\.jar", "application.jar", "bbj/webforj.bbj"),
          zip.stream().map(ZipEntry::getName).toList());

      try (InputStream input = zip.getInputStream(zip.getEntry("application.jar"))) {
        assertArrayEquals(Files.readAllBytes(app.resolve("target/hello-app-1.0-SNAPSHOT.jar")),
            input.readAllBytes());
      }
    }
  }

  @Test
  void shouldPackBbjProgramsOfWebforjDependenciesOnly() throws Exception {
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
    Files.createDirectories(app.resolve("src/main/resources/bbj"));
    Files.writeString(app.resolve("src/main/resources/bbj/helper.bbj"), "rem helper");
    Files.createDirectories(app.resolve("src/main/resources/legacy/nested"));
    Files.writeString(app.resolve("src/main/resources/legacy/nested/helper.BBJ"), "rem nested");
    Files.writeString(app.resolve("src/main/resources/bbj/readme.txt"), "not a program");
    Path helper = Files.copy(HelloApp.writeJar(app), temporary.resolve("webforj-helper.jar"));
    Path extra = Files.copy(helper, temporary.resolve("webforj-extra.jar"));
    Path foundation =
        Path.of(App.class.getProtectionDomain().getCodeSource().getLocation().toURI());
    ApplicationClasspath classpath = new ApplicationClasspath(
        List.of(new ResolvedJar("com.webforj", "webforj-foundation", "26.03", foundation),
            new ResolvedJar("com.webforj", "webforj-helper", "1.0", helper),
            new ResolvedJar("org.example", "webforj-extra", "1.0", extra)));
    Path archive = temporary.resolve("hello-app.zip");

    Deployment.create(archive).setApplication(HelloApp.writeJar(app)).setClasspath(classpath)
        .build().writeArchive();

    try (ZipFile zip = new ZipFile(archive.toFile())) {
      assertEquals(
          List.of("dependencies/00000-" + foundation.getFileName(),
              "dependencies/00001-webforj-helper.jar", "dependencies/00002-webforj-extra.jar",
              "application.jar", "bbj/webforj.bbj", "bbj/helper.bbj", "legacy/nested/helper.BBJ"),
          zip.stream().map(ZipEntry::getName).toList());
    }
  }

  @Test
  void shouldRefuseTheSameBbjProgramFromTwoWebforjDependencies() throws Exception {
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
    Files.createDirectories(app.resolve("src/main/resources/bbj"));
    Files.writeString(app.resolve("src/main/resources/bbj/shared.bbj"), "rem shared");
    Path first = Files.copy(HelloApp.writeJar(app), temporary.resolve("webforj-first.jar"));
    Path second = Files.copy(HelloApp.writeJar(app), temporary.resolve("webforj-second.jar"));
    Path foundation =
        Path.of(App.class.getProtectionDomain().getCodeSource().getLocation().toURI());
    ApplicationClasspath classpath = new ApplicationClasspath(
        List.of(new ResolvedJar("com.webforj", "webforj-foundation", "26.03", foundation),
            new ResolvedJar("com.webforj", "webforj-first", "1.0", first),
            new ResolvedJar("com.webforj", "webforj-second", "1.0", second)));
    Path archive = temporary.resolve("hello-app.zip");
    Deployment deployment = Deployment.create(archive).setApplication(HelloApp.writeJar(app))
        .setClasspath(classpath).build();

    IOException failure = assertThrows(IOException.class, deployment::writeArchive);

    assertEquals("Duplicate BBj deployment program entry: bbj/shared.bbj", failure.getMessage());
    assertEquals(Set.of("hello-app", "webforj-first.jar", "webforj-second.jar"),
        Set.of(temporary.toFile().list()));
  }

  @Test
  void shouldRefuseDependencyWithoutFile() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
    ApplicationClasspath classpath = new ApplicationClasspath(
        List.of(new ResolvedJar("com.webforj", "webforj-foundation", "26.03", null)));
    Path archive = temporary.resolve("hello-app.zip");
    Deployment deployment = Deployment.create(archive).setApplication(HelloApp.writeJar(app))
        .setClasspath(classpath).build();

    IOException failure = assertThrows(IOException.class, deployment::writeArchive);

    assertEquals("BBj deployment dependency is unresolved. Resolve the runtime classpath",
        failure.getMessage());
    assertFalse(Files.exists(archive));
  }

  @Test
  void shouldRefuseWebforjEngineOnTheClasspath() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
    Path foundation = temporary.resolve("webforj-foundation.jar");
    Path engine = temporary.resolve("webforj-engine.jar");
    Files.createFile(foundation);
    Files.createFile(engine);
    ApplicationClasspath classpath = new ApplicationClasspath(
        List.of(new ResolvedJar("com.webforj", "webforj-foundation", "26.03", foundation),
            new ResolvedJar("com.webforj", "webforj-engine", "26.03", engine)));
    Path archive = temporary.resolve("hello-app.zip");
    Deployment deployment = Deployment.create(archive).setApplication(HelloApp.writeJar(app))
        .setClasspath(classpath).build();

    IOException failure = assertThrows(IOException.class, deployment::writeArchive);

    assertEquals("BBj deployment must not carry webforj-engine, BBj is the engine. Exclude it "
        + "from the webforj dependency", failure.getMessage());
    assertFalse(Files.exists(archive));
  }

  @Test
  void shouldRefuseClasspathWithoutWebforjFoundation() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
    Path archive = temporary.resolve("hello-app.zip");
    Deployment deployment = Deployment.create(archive).setApplication(HelloApp.writeJar(app))
        .setClasspath(new ApplicationClasspath(List.of())).build();

    IOException failure = assertThrows(IOException.class, deployment::writeArchive);

    assertEquals("BBj deployment requires webforj-foundation on the runtime classpath",
        failure.getMessage());
    assertFalse(Files.exists(archive));
  }

  @Test
  void shouldRefuseDeploymentWithoutApplicationJar() throws Exception {
    Path foundation =
        Path.of(App.class.getProtectionDomain().getCodeSource().getLocation().toURI());
    ApplicationClasspath classpath = new ApplicationClasspath(
        List.of(new ResolvedJar("com.webforj", "webforj-foundation", "26.03", foundation)));
    Path archive = temporary.resolve("hello-app.zip");
    Deployment deployment = Deployment.create(archive).setClasspath(classpath).build();

    IOException failure = assertThrows(IOException.class, deployment::writeArchive);

    assertEquals("BBj deployment application JAR is missing. Package the application first",
        failure.getMessage());
    assertFalse(Files.exists(archive));
  }

  @Test
  void shouldPackDevelopmentPropertiesInPlaceOfTheApplicationJar() throws Exception {
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
    Path classes = Files.createDirectories(app.resolve("target/../target/classes"));
    Path foundation =
        Path.of(App.class.getProtectionDomain().getCodeSource().getLocation().toURI());
    ApplicationClasspath classpath = new ApplicationClasspath(
        List.of(new ResolvedJar("com.webforj", "webforj-foundation", "26.03", foundation)));
    Path archive = temporary.resolve("hello-app.zip");

    Deployment.create(archive).setClasspath(classpath).setClassFolders(List.of(classes)).build()
        .writeArchive();

    try (ZipFile zip = new ZipFile(archive.toFile())) {
      assertEquals(List.of("dependencies/00000-" + foundation.getFileName(),
          "development.properties", "bbj/webforj.bbj"),
          zip.stream().map(ZipEntry::getName).toList());
      try (InputStream input = zip.getInputStream(zip.getEntry("development.properties"))) {
        String text = new String(input.readAllBytes(), StandardCharsets.UTF_8);
        assertEquals("classFolder=" + app.resolve("target/classes").toAbsolutePath()
            + System.lineSeparator(), text.substring(text.indexOf('\n') + 1));
      }
    }

    assertEquals(Set.of("hello-app", "hello-app.zip"), Set.of(temporary.toFile().list()));
  }

  @Test
  void shouldNameEveryClassFolderInOrderSeparatedByThePathSeparator() throws Exception {
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
    Path classes = Files.createDirectories(app.resolve("build/classes/java/main"));
    Path resources = Files.createDirectories(app.resolve("build/../build/resources/main"));
    Path foundation =
        Path.of(App.class.getProtectionDomain().getCodeSource().getLocation().toURI());
    ApplicationClasspath classpath = new ApplicationClasspath(
        List.of(new ResolvedJar("com.webforj", "webforj-foundation", "26.03", foundation)));
    Path archive = temporary.resolve("hello-app.zip");

    Deployment.create(archive).setClasspath(classpath).setClassFolders(List.of(classes, resources))
        .build().writeArchive();

    Properties properties = new Properties();
    try (ZipFile zip = new ZipFile(archive.toFile());
        InputStream input = zip.getInputStream(zip.getEntry("development.properties"))) {
      properties.load(input);
    }

    assertEquals(
        classes.toAbsolutePath() + File.pathSeparator
            + app.resolve("build/resources/main").toAbsolutePath(),
        properties.getProperty("classFolder"));
  }

  @Test
  void shouldRefuseDeploymentWithApplicationJarAndClassFolder() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
    Deployment.Builder builder =
        Deployment.create(temporary.resolve("hello-app.zip")).setApplication(HelloApp.writeJar(app))
            .setClassFolders(List.of(app.resolve("target/classes")));

    IllegalStateException failure = assertThrows(IllegalStateException.class, builder::build);

    assertEquals("a deployment carries an application jar or class folders, not both",
        failure.getMessage());
  }

  @Test
  void shouldRefuseEmptyClassFolders() {
    Deployment.Builder builder = Deployment.create(temporary.resolve("hello-app.zip"));
    List<Path> none = List.of();

    IllegalArgumentException failure =
        assertThrows(IllegalArgumentException.class, () -> builder.setClassFolders(none));

    assertEquals("a development run needs at least one class folder", failure.getMessage());
  }

  @Test
  void shouldRefuseWebforjFoundationWithoutLauncher() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
    Path jar = HelloApp.writeJar(app);
    ApplicationClasspath classpath = new ApplicationClasspath(
        List.of(new ResolvedJar("com.webforj", "webforj-foundation", "26.03", jar)));
    Path archive = temporary.resolve("hello-app.zip");
    Deployment deployment =
        Deployment.create(archive).setApplication(jar).setClasspath(classpath).build();

    IOException failure = assertThrows(IOException.class, deployment::writeArchive);

    assertEquals("The webforj-foundation dependency does not contain the BBj launcher",
        failure.getMessage());
    assertFalse(Files.exists(archive));
  }

  @Test
  void shouldKeepClasspathOrderWhenMoreThanTenDependenciesArePacked() throws Exception {
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
    Path jar = HelloApp.writeJar(app);
    Path foundation =
        Path.of(App.class.getProtectionDomain().getCodeSource().getLocation().toURI());
    List<ResolvedJar> jars = new ArrayList<>(
        List.of(new ResolvedJar("com.webforj", "webforj-foundation", "26.03", foundation)));
    for (int index = 1; index <= 11; index++) {
      Path dependency = Files.copy(jar, temporary.resolve("dependency-" + index + ".jar"));
      jars.add(new ResolvedJar("org.example", "dependency-" + index, "1.0", dependency));
    }

    Path archive = temporary.resolve("hello-app.zip");

    Deployment.create(archive).setApplication(jar).setClasspath(new ApplicationClasspath(jars))
        .build().writeArchive();

    try (ZipFile zip = new ZipFile(archive.toFile())) {
      assertEquals(
          List.of("dependencies/00000-" + foundation.getFileName(),
              "dependencies/00001-dependency-1.jar", "dependencies/00002-dependency-2.jar",
              "dependencies/00003-dependency-3.jar", "dependencies/00004-dependency-4.jar",
              "dependencies/00005-dependency-5.jar", "dependencies/00006-dependency-6.jar",
              "dependencies/00007-dependency-7.jar", "dependencies/00008-dependency-8.jar",
              "dependencies/00009-dependency-9.jar", "dependencies/00010-dependency-10.jar",
              "dependencies/00011-dependency-11.jar", "application.jar", "bbj/webforj.bbj"),
          zip.stream().map(ZipEntry::getName).toList());
    }
  }
}
