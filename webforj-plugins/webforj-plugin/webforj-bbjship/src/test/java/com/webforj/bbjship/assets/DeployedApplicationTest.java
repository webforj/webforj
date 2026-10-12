package com.webforj.bbjship.assets;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.webforj.plugin.foundation.bbj.DeploymentArchive;
import com.webforj.plugin.foundation.bbj.HelloApp;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

final class DeployedApplicationTest {

  @TempDir
  Path temporary;

  @Test
  void shouldReadTheAssetConfigurationOfTheApplicationJar() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
    Files.writeString(app.resolve("src/main/resources/webforj.conf"), """
        webforj {
          entry = "org.example.Application"
          assetsDir = "public"
          iconsDir = "brand"
          assetsIndex = ["home.html"]
          assetsExt = [".html"]
          assetsCacheControl = "max-age=60"
          mime.extensions { css = "text/x-css" }
        }
        """);
    Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
    Path directory = temporary.resolve("deployment");
    DeploymentArchive.read(archive, directory);

    DeployedApplication application = DeployedApplication.load(directory);

    assertEquals("public/", application.getAssetsPrefix());
    assertEquals("brand/", application.getIconsPrefix());
    assertEquals(List.of("home.html"), application.getIndexes());
    assertEquals(List.of("", ".html"), application.getExtensions());
    assertEquals("max-age=60", application.getCacheControl());
    assertEquals(Map.of("css", "text/x-css"), application.getMimeExtensions());
  }

  @Test
  void shouldFailWhenTheApplicationJarIsMissing() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
    Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
    Path directory = temporary.resolve("deployment");
    DeploymentArchive.read(archive, directory);
    Files.delete(directory.resolve("application.jar"));

    IOException failure =
        assertThrows(IOException.class, () -> DeployedApplication.load(directory));

    assertEquals("BBj deployment application JAR is missing", failure.getMessage());
  }

  @Test
  void shouldReadTheApplicationJarBeforeTheDependencyJars() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
    Files.createDirectories(app.resolve("src/main/resources/static/webforj/icon-badge"));
    Files.writeString(app.resolve("src/main/resources/static/webforj/icon-badge/icon-badge.js"),
        "console.log('hello-app');\n");
    Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
    Path directory = temporary.resolve("deployment");
    DeploymentArchive.read(archive, directory);
    byte[] foundation;
    try (InputStream input = getClass().getClassLoader()
        .getResourceAsStream("static/webforj/icon-badge/icon-badge.min.js")) {
      foundation = input.readAllBytes();
    }

    DeployedApplication application = DeployedApplication.load(directory);

    assertArrayEquals("console.log('hello-app');\n".getBytes(StandardCharsets.UTF_8),
        application.readResource("static/webforj/icon-badge/icon-badge.js").orElseThrow());
    assertArrayEquals(foundation,
        application.readResource("static/webforj/icon-badge/icon-badge.min.js").orElseThrow());
  }

  @Test
  void shouldReadDependencyResourceAfterTheApplicationJarIsDeleted() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
    Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
    Path directory = temporary.resolve("deployment");
    DeploymentArchive.read(archive, directory);
    byte[] foundation;
    try (InputStream input = getClass().getClassLoader()
        .getResourceAsStream("static/webforj/icon-badge/icon-badge.min.js")) {
      foundation = input.readAllBytes();
    }

    DeployedApplication application = DeployedApplication.load(directory);
    Files.delete(directory.resolve("application.jar"));

    assertArrayEquals(foundation,
        application.readResource("static/webforj/icon-badge/icon-badge.min.js").orElseThrow());
  }

  @Test
  void shouldReadTheClassFolderOfDevelopmentRun() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
    Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
    Path directory = temporary.resolve("deployment");
    DeploymentArchive.read(archive, directory);
    Files.delete(directory.resolve("application.jar"));
    Path classFolder = app.resolve("src/main/resources");
    Files.writeString(classFolder.resolve("webforj.conf"), """
        webforj {
          entry = "org.example.Application"
          assetsCacheControl = "max-age=60"
        }
        """);
    Files.writeString(directory.resolve("development"), classFolder.toString());

    DeployedApplication application = DeployedApplication.load(directory);

    assertEquals("max-age=60", application.getCacheControl());
    assertArrayEquals(Files.readAllBytes(HelloApp.getFile("static/app.css")),
        application.readResource("static/app.css").orElseThrow());
    assertArrayEquals(Files.readAllBytes(HelloApp.getFile("icons/icon.png")),
        application.readResource("icons/icon.png").orElseThrow());
  }

  @Test
  void shouldLoadDevelopmentRunWithMissingClassFolder() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
    Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
    Path directory = temporary.resolve("deployment");
    DeploymentArchive.read(archive, directory);
    Files.delete(directory.resolve("application.jar"));
    Path removed = temporary.resolve("removed-classes");
    Path classFolder = app.resolve("src/main/resources");
    Files.writeString(classFolder.resolve("webforj.conf"), """
        webforj.assetsCacheControl = "max-age=60"
        """);
    Files.writeString(directory.resolve("development"), removed + "\n" + classFolder + "\n");

    DeployedApplication application = DeployedApplication.load(directory);

    assertEquals("max-age=60", application.getCacheControl());
  }

  @Test
  void shouldReadTheClassFolderOfDevelopmentRunBeforeTheDependencyJars() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
    Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
    Path directory = temporary.resolve("deployment");
    DeploymentArchive.read(archive, directory);
    Files.delete(directory.resolve("application.jar"));
    Path classFolder = app.resolve("src/main/resources");
    Files.createDirectories(classFolder.resolve("static/webforj/icon-badge"));
    Files.writeString(classFolder.resolve("static/webforj/icon-badge/icon-badge.min.js"),
        "console.log('hello-app');\n");
    Files.writeString(directory.resolve("development"), classFolder.toString());
    byte[] foundation;
    try (InputStream input = getClass().getClassLoader()
        .getResourceAsStream("static/webforj/icon-badge/icon-badge.js")) {
      foundation = input.readAllBytes();
    }

    DeployedApplication application = DeployedApplication.load(directory);

    assertArrayEquals("console.log('hello-app');\n".getBytes(StandardCharsets.UTF_8),
        application.readResource("static/webforj/icon-badge/icon-badge.min.js").orElseThrow());
    assertArrayEquals(foundation,
        application.readResource("static/webforj/icon-badge/icon-badge.js").orElseThrow());
  }

  @Test
  void shouldReadEveryClassFolderOfDevelopmentRunInOrder() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
    Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
    Path directory = temporary.resolve("deployment");
    DeploymentArchive.read(archive, directory);
    Files.delete(directory.resolve("application.jar"));
    Path classes = app.resolve("src/main/java");
    Path resources = app.resolve("src/main/resources");
    Files.writeString(resources.resolve("webforj.conf"), """
        webforj {
          entry = "org.example.Application"
          assetsCacheControl = "max-age=60"
        }
        """);
    Files.writeString(directory.resolve("development"), """
        %s
        %s
        """.formatted(classes, resources));

    DeployedApplication application = DeployedApplication.load(directory);

    assertEquals("max-age=60", application.getCacheControl());
    assertArrayEquals(Files.readAllBytes(classes.resolve("org/example/Application.java")),
        application.readResource("org/example/Application.java").orElseThrow());
    assertArrayEquals(Files.readAllBytes(HelloApp.getFile("static/app.css")),
        application.readResource("static/app.css").orElseThrow());
  }

  @Test
  void shouldReadTheChangedFileOfTheClassFolderAfterLoad() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
    Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
    Path directory = temporary.resolve("deployment");
    DeploymentArchive.read(archive, directory);
    Files.delete(directory.resolve("application.jar"));
    Path classFolder = app.resolve("src/main/resources");
    Files.writeString(directory.resolve("development"), classFolder.toString());
    DeployedApplication application = DeployedApplication.load(directory);
    byte[] loaded = application.readResource("static/app.css").orElseThrow();

    Files.writeString(classFolder.resolve("static/app.css"), "body { color: red; }\n");

    assertArrayEquals(Files.readAllBytes(HelloApp.getFile("static/app.css")), loaded);
    assertArrayEquals("body { color: red; }\n".getBytes(StandardCharsets.UTF_8),
        application.readResource("static/app.css").orElseThrow());
  }

  @Test
  void shouldReadEmptyFileAsPresentWithoutBytes() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
    Path generated = app.resolve("src/main/resources/static/frontend/generated");
    Files.createDirectories(generated);
    Files.write(generated.resolve("bundle.js"), new byte[0]);
    Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
    Path directory = temporary.resolve("deployment");
    DeploymentArchive.read(archive, directory);

    DeployedApplication application = DeployedApplication.load(directory);

    assertArrayEquals(new byte[0],
        application.readResource("static/frontend/generated/bundle.js").orElseThrow());
  }

  @ParameterizedTest
  @MethodSource("getUnreadableResources")
  void shouldReadNothingForMissingFilesAndFolders(String resource) throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
    Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
    Path directory = temporary.resolve("deployment");
    DeploymentArchive.read(archive, directory);

    DeployedApplication application = DeployedApplication.load(directory);

    assertEquals(Optional.empty(), application.readResource(resource));
  }

  @ParameterizedTest
  @MethodSource("getFolderNames")
  void shouldReadNothingForFolderOfTheClassFolderOfDevelopmentRun(String resource)
      throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
    Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
    Path directory = temporary.resolve("deployment");
    DeploymentArchive.read(archive, directory);
    Files.delete(directory.resolve("application.jar"));
    Files.writeString(directory.resolve("development"),
        app.resolve("src/main/resources").toString());

    DeployedApplication application = DeployedApplication.load(directory);

    assertEquals(Optional.empty(), application.readResource(resource));
  }

  @Test
  void shouldReadNothingOnceTheApplicationIsClosed() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
    Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
    Path directory = temporary.resolve("deployment");
    DeploymentArchive.read(archive, directory);
    DeployedApplication application = DeployedApplication.load(directory);
    byte[] open = application.readResource("static/app.css").orElseThrow();

    application.close();

    assertArrayEquals(Files.readAllBytes(HelloApp.getFile("static/app.css")), open);
    assertEquals(Optional.empty(), application.readResource("static/app.css"));
  }

  private static Stream<String> getUnreadableResources() {
    return Stream.of("static/missing.css", "static/webforj/");
  }

  private static Stream<String> getFolderNames() {
    return Stream.of("static", "static/");
  }
}
