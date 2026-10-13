package com.webforj.bbjship.assets;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.bbjship.DevelopmentRunFiles;
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
import java.util.concurrent.atomic.AtomicInteger;
import java.util.jar.JarFile;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

final class DeployedApplicationTest {

  private static final String APP_NAME = "hello-app";
  private static final String ARCHIVE_NAME = "hello-app.zip";
  private static final String DEPLOYMENT_FOLDER = "deployment";
  private static final String APPLICATION_JAR = "application.jar";
  private static final String APP_CSS = "static/app.css";
  private static final String BADGE_SCRIPT = "static/webforj/icon-badge/icon-badge.js";
  private static final String MINIFIED_BADGE_SCRIPT = "static/webforj/icon-badge/icon-badge.min.js";
  private static final String BADGE_SCRIPT_SOURCE = "console.log('hello-app');\n";
  private static final String RESOURCES_FOLDER = "src/main/resources";
  private static final String CONF_FILE = "webforj.conf";
  private static final String CACHE_CONTROL = "max-age=60";

  @TempDir
  Path temporary;

  @Test
  void shouldReadTheAssetConfigurationOfTheApplicationJar() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
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
    Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
    Path directory = temporary.resolve(DEPLOYMENT_FOLDER);
    DeploymentArchive.read(archive, directory);

    DeployedApplication application = DeployedApplication.load(directory, new ResourceIndexes());

    assertEquals("public/", application.getAssetsPrefix());
    assertEquals("brand/", application.getIconsPrefix());
    assertEquals(List.of("home.html"), application.getIndexes());
    assertEquals(List.of("", ".html"), application.getExtensions());
    assertEquals(CACHE_CONTROL, application.getCacheControl());
    assertEquals(Map.of("css", "text/x-css"), application.getMimeExtensions());
  }

  @Test
  void shouldFailWhenTheApplicationJarIsMissing() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
    Path directory = temporary.resolve(DEPLOYMENT_FOLDER);
    DeploymentArchive.read(archive, directory);
    Files.delete(directory.resolve(APPLICATION_JAR));

    IOException failure = assertThrows(IOException.class,
        () -> DeployedApplication.load(directory, new ResourceIndexes()));

    assertEquals("BBj deployment application JAR is missing", failure.getMessage());
  }

  @Test
  void shouldReadTheApplicationJarBeforeTheDependencyJars() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Files.createDirectories(app.resolve("src/main/resources/static/webforj/icon-badge"));
    Files.writeString(app.resolve("src/main/resources/static/webforj/icon-badge/icon-badge.js"),
        BADGE_SCRIPT_SOURCE);
    Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
    Path directory = temporary.resolve(DEPLOYMENT_FOLDER);
    DeploymentArchive.read(archive, directory);
    byte[] foundation;
    try (InputStream input =
        getClass().getClassLoader().getResourceAsStream(MINIFIED_BADGE_SCRIPT)) {
      foundation = input.readAllBytes();
    }

    DeployedApplication application = DeployedApplication.load(directory, new ResourceIndexes());

    assertArrayEquals(BADGE_SCRIPT_SOURCE.getBytes(StandardCharsets.UTF_8),
        application.readResource(BADGE_SCRIPT).orElseThrow());
    assertArrayEquals(foundation, application.readResource(MINIFIED_BADGE_SCRIPT).orElseThrow());
  }

  @Test
  void shouldReadDependencyResourceAfterTheApplicationJarIsDeleted() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
    Path directory = temporary.resolve(DEPLOYMENT_FOLDER);
    DeploymentArchive.read(archive, directory);
    byte[] foundation;
    try (InputStream input =
        getClass().getClassLoader().getResourceAsStream(MINIFIED_BADGE_SCRIPT)) {
      foundation = input.readAllBytes();
    }

    DeployedApplication application = DeployedApplication.load(directory, new ResourceIndexes());
    Files.delete(directory.resolve(APPLICATION_JAR));

    assertArrayEquals(foundation, application.readResource(MINIFIED_BADGE_SCRIPT).orElseThrow());
  }

  @Test
  void shouldReadTheClassFolderOfDevelopmentRun() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
    Path directory = temporary.resolve(DEPLOYMENT_FOLDER);
    DeploymentArchive.read(archive, directory);
    Files.delete(directory.resolve(APPLICATION_JAR));
    Path classFolder = app.resolve(RESOURCES_FOLDER);
    Files.writeString(classFolder.resolve(CONF_FILE), """
        webforj {
          entry = "org.example.Application"
          assetsCacheControl = "max-age=60"
        }
        """);
    DevelopmentRunFiles.write(directory, classFolder);

    DeployedApplication application = DeployedApplication.load(directory, new ResourceIndexes());

    assertEquals(CACHE_CONTROL, application.getCacheControl());
    assertArrayEquals(Files.readAllBytes(HelloApp.getFile(APP_CSS)),
        application.readResource(APP_CSS).orElseThrow());
    assertArrayEquals(Files.readAllBytes(HelloApp.getFile("icons/icon.png")),
        application.readResource("icons/icon.png").orElseThrow());
  }

  @Test
  void shouldLoadDevelopmentRunWithMissingClassFolder() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
    Path directory = temporary.resolve(DEPLOYMENT_FOLDER);
    DeploymentArchive.read(archive, directory);
    Files.delete(directory.resolve(APPLICATION_JAR));
    Path removed = temporary.resolve("removed-classes");
    Path classFolder = app.resolve(RESOURCES_FOLDER);
    Files.writeString(classFolder.resolve(CONF_FILE), """
        webforj.assetsCacheControl = "max-age=60"
        """);
    DevelopmentRunFiles.write(directory, removed, classFolder);

    DeployedApplication application = DeployedApplication.load(directory, new ResourceIndexes());

    assertEquals(CACHE_CONTROL, application.getCacheControl());
  }

  @Test
  void shouldReadTheClassFolderOfDevelopmentRunBeforeTheDependencyJars() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
    Path directory = temporary.resolve(DEPLOYMENT_FOLDER);
    DeploymentArchive.read(archive, directory);
    Files.delete(directory.resolve(APPLICATION_JAR));
    Path classFolder = app.resolve(RESOURCES_FOLDER);
    Files.createDirectories(classFolder.resolve("static/webforj/icon-badge"));
    Files.writeString(classFolder.resolve(MINIFIED_BADGE_SCRIPT), BADGE_SCRIPT_SOURCE);
    DevelopmentRunFiles.write(directory, classFolder);
    byte[] foundation;
    try (InputStream input = getClass().getClassLoader().getResourceAsStream(BADGE_SCRIPT)) {
      foundation = input.readAllBytes();
    }

    DeployedApplication application = DeployedApplication.load(directory, new ResourceIndexes());

    assertArrayEquals(BADGE_SCRIPT_SOURCE.getBytes(StandardCharsets.UTF_8),
        application.readResource(MINIFIED_BADGE_SCRIPT).orElseThrow());
    assertArrayEquals(foundation, application.readResource(BADGE_SCRIPT).orElseThrow());
  }

  @Test
  void shouldReadEveryClassFolderOfDevelopmentRunInOrder() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
    Path directory = temporary.resolve(DEPLOYMENT_FOLDER);
    DeploymentArchive.read(archive, directory);
    Files.delete(directory.resolve(APPLICATION_JAR));
    Path classes = app.resolve("src/main/java");
    Path resources = app.resolve(RESOURCES_FOLDER);
    Files.writeString(resources.resolve(CONF_FILE), """
        webforj {
          entry = "org.example.Application"
          assetsCacheControl = "max-age=60"
        }
        """);
    DevelopmentRunFiles.write(directory, classes, resources);

    DeployedApplication application = DeployedApplication.load(directory, new ResourceIndexes());

    assertEquals(CACHE_CONTROL, application.getCacheControl());
    assertArrayEquals(Files.readAllBytes(classes.resolve("org/example/Application.java")),
        application.readResource("org/example/Application.java").orElseThrow());
    assertArrayEquals(Files.readAllBytes(HelloApp.getFile(APP_CSS)),
        application.readResource(APP_CSS).orElseThrow());
  }

  @Test
  void shouldReadTheChangedFileOfTheClassFolderAfterLoad() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
    Path directory = temporary.resolve(DEPLOYMENT_FOLDER);
    DeploymentArchive.read(archive, directory);
    Files.delete(directory.resolve(APPLICATION_JAR));
    Path classFolder = app.resolve(RESOURCES_FOLDER);
    DevelopmentRunFiles.write(directory, classFolder);
    DeployedApplication application = DeployedApplication.load(directory, new ResourceIndexes());
    byte[] loaded = application.readResource(APP_CSS).orElseThrow();

    Files.writeString(classFolder.resolve(APP_CSS), "body { color: red; }\n");

    assertArrayEquals(Files.readAllBytes(HelloApp.getFile(APP_CSS)), loaded);
    assertArrayEquals("body { color: red; }\n".getBytes(StandardCharsets.UTF_8),
        application.readResource(APP_CSS).orElseThrow());
  }

  @Test
  void shouldReadEmptyFileAsPresentWithoutBytes() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Path generated = app.resolve("src/main/resources/static/frontend/generated");
    Files.createDirectories(generated);
    Files.write(generated.resolve("bundle.js"), new byte[0]);
    Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
    Path directory = temporary.resolve(DEPLOYMENT_FOLDER);
    DeploymentArchive.read(archive, directory);

    DeployedApplication application = DeployedApplication.load(directory, new ResourceIndexes());

    assertArrayEquals(new byte[0],
        application.readResource("static/frontend/generated/bundle.js").orElseThrow());
  }

  @ParameterizedTest
  @MethodSource("getUnreadableResources")
  void shouldReadNothingForMissingFilesAndFolders(String resource) throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
    Path directory = temporary.resolve(DEPLOYMENT_FOLDER);
    DeploymentArchive.read(archive, directory);

    DeployedApplication application = DeployedApplication.load(directory, new ResourceIndexes());

    assertEquals(Optional.empty(), application.readResource(resource));
  }

  @ParameterizedTest
  @MethodSource("getFolderNames")
  void shouldReadNothingForFolderOfTheClassFolderOfDevelopmentRun(String resource)
      throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
    Path directory = temporary.resolve(DEPLOYMENT_FOLDER);
    DeploymentArchive.read(archive, directory);
    Files.delete(directory.resolve(APPLICATION_JAR));
    DevelopmentRunFiles.write(directory, app.resolve(RESOURCES_FOLDER));

    DeployedApplication application = DeployedApplication.load(directory, new ResourceIndexes());

    assertEquals(Optional.empty(), application.readResource(resource));
  }

  @Test
  void shouldScanTheJarsOnceForSeveralLoadsOfTheSameDeployment() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
    Path directory = temporary.resolve(DEPLOYMENT_FOLDER);
    DeploymentArchive.read(archive, directory);
    AtomicInteger opened = new AtomicInteger();
    ResourceIndexes indexes = new ResourceIndexes(jar -> {
      opened.incrementAndGet();
      return new JarFile(jar.toFile());
    });

    DeployedApplication.load(directory, indexes);
    int scanned = opened.get();
    for (int request = 0; request < 3; request++) {
      DeployedApplication.load(directory, indexes).readResource(APP_CSS).orElseThrow();
    }

    assertTrue(scanned > 0);
    assertEquals(scanned + 3, opened.get());
    assertEquals(1, indexes.size());
  }

  private static Stream<String> getUnreadableResources() {
    return Stream.of("static/missing.css", "static/webforj/");
  }

  private static Stream<String> getFolderNames() {
    return Stream.of("static", "static/");
  }
}
