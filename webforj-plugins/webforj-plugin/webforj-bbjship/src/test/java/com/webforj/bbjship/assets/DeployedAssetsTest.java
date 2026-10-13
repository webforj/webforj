package com.webforj.bbjship.assets;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.bbjship.DevelopmentRunFiles;
import com.webforj.bbjship.assets.model.AssetRequest;
import com.webforj.bbjship.assets.model.AssetResponse;
import com.webforj.plugin.foundation.bbj.DeploymentArchive;
import com.webforj.plugin.foundation.bbj.FileTree;
import com.webforj.plugin.foundation.bbj.HelloApp;
import java.awt.Color;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.jar.JarFile;
import java.util.stream.Stream;
import javax.imageio.ImageIO;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

final class DeployedAssetsTest {

  private static final String APP_NAME = "hello-app";
  private static final String ARCHIVE_NAME = "hello-app.zip";
  private static final String DEPLOYMENTS_FOLDER = "deployments";
  private static final String CURRENT_FILE = "hello-app/current";
  private static final String APP_CSS = "static/app.css";
  private static final String DOCS_FOLDER = "static/docs";
  private static final String CONF_PATH = "src/main/resources/webforj.conf";
  private static final String APP_CSS_PATH = "src/main/resources/static/app.css";
  private static final String APP_CSS_SOURCE = "body {}\n";
  private static final String PNG_CONTENT_TYPE = "image/png";
  private static final String ICON = "icons/icon.png";
  private static final String SIZED_ICON = "icons/icon-20x20.png";

  @Nested
  class StaticFiles {

    @TempDir
    Path temporary;

    @Test
    void shouldAnswerNotFoundOnceTheDeploymentFolderIsRemoved() throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
      Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
      Path root = temporary.resolve(DEPLOYMENTS_FOLDER);
      String id = UUID.randomUUID().toString();
      Path deployment = Files.createDirectories(root.resolve(APP_NAME)).resolve(id);
      DeploymentArchive.read(archive, deployment);
      Files.writeString(root.resolve(CURRENT_FILE), id);
      AssetRequest request = AssetRequest.create(APP_NAME, APP_CSS).build();
      AssetResponse served = DeployedAssets.read(root.toString(), request);

      FileTree.removeDirectory(deployment);

      assertEquals(200, served.getStatus());
      assertEquals(404, DeployedAssets.read(root.toString(), request).getStatus());
    }

    @Test
    void shouldDropTheIndexOfTheDeploymentFolderOnceItIsRemoved() throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
      Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
      Path root = temporary.resolve(DEPLOYMENTS_FOLDER);
      String id = UUID.randomUUID().toString();
      Path deployment = Files.createDirectories(root.resolve(APP_NAME)).resolve(id);
      DeploymentArchive.read(archive, deployment);
      Files.writeString(root.resolve(CURRENT_FILE), id);
      AssetRequest request = AssetRequest.create(APP_NAME, APP_CSS).build();
      ResourceIndexes indexes = new ResourceIndexes();
      DeployedAssets.read(root.toString(), request, indexes);
      int held = indexes.size();

      FileTree.removeDirectory(deployment);
      DeployedAssets.read(root.toString(), request, indexes);

      assertEquals(1, held);
      assertEquals(0, indexes.size());
    }

    @Test
    void shouldScanTheJarsOnceForSeveralRequests() throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
      Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
      Path root = temporary.resolve(DEPLOYMENTS_FOLDER);
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive, Files.createDirectories(root.resolve(APP_NAME)).resolve(id));
      Files.writeString(root.resolve(CURRENT_FILE), id);
      AtomicInteger opened = new AtomicInteger();
      ResourceIndexes indexes = new ResourceIndexes(jar -> {
        opened.incrementAndGet();
        return new JarFile(jar.toFile());
      });
      AssetRequest request = AssetRequest.create(APP_NAME, APP_CSS).build();

      DeployedAssets.read(root.toString(), request, indexes);
      int afterFirst = opened.get();
      DeployedAssets.read(root.toString(), request, indexes);
      DeployedAssets.read(root.toString(), request, indexes);

      assertTrue(afterFirst > 0);
      assertEquals(afterFirst + 2, opened.get());
    }

    @Test
    void shouldServeTheStylesheetOfTheApplicationWithoutCacheControl() throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
      Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
      Path root = temporary.resolve(DEPLOYMENTS_FOLDER);
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive, Files.createDirectories(root.resolve(APP_NAME)).resolve(id));
      Files.writeString(root.resolve(CURRENT_FILE), id);

      AssetResponse response =
          DeployedAssets.read(root.toString(), AssetRequest.create(APP_NAME, APP_CSS).build());

      assertEquals(200, response.getStatus());
      assertEquals("text/css", response.getContentType());
      assertNull(response.getCacheControl());
      assertNull(response.getEntityTag());
      assertArrayEquals(Files.readAllBytes(HelloApp.getFile(APP_CSS)), response.getBody());
    }

    @Test
    void shouldServeTheStylesheetOfTheClassFolderOfDevelopmentRun() throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
      Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
      Path root = temporary.resolve(DEPLOYMENTS_FOLDER);
      String id = UUID.randomUUID().toString();
      Path directory = Files.createDirectories(root.resolve(APP_NAME)).resolve(id);
      DeploymentArchive.read(archive, directory);
      Files.delete(directory.resolve("application.jar"));
      Path classFolder = app.resolve("src/main/resources");
      Files.writeString(classFolder.resolve(APP_CSS), "body { color: red; }\n");
      DevelopmentRunFiles.write(directory, classFolder);
      Files.writeString(root.resolve(CURRENT_FILE), id);

      AssetResponse response =
          DeployedAssets.read(root.toString(), AssetRequest.create(APP_NAME, APP_CSS).build());

      assertEquals(200, response.getStatus());
      assertEquals("text/css", response.getContentType());
      assertArrayEquals("body { color: red; }\n".getBytes(StandardCharsets.UTF_8),
          response.getBody());
    }

    @Test
    void shouldAnswerNotFoundForFolderWithoutIndexOfTheClassFolderOfDevelopmentRun()
        throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
      Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
      Path root = temporary.resolve(DEPLOYMENTS_FOLDER);
      String id = UUID.randomUUID().toString();
      Path directory = Files.createDirectories(root.resolve(APP_NAME)).resolve(id);
      DeploymentArchive.read(archive, directory);
      Files.delete(directory.resolve("application.jar"));
      Path classFolder = app.resolve("src/main/resources");
      Files.createDirectories(classFolder.resolve(DOCS_FOLDER));
      Files.writeString(classFolder.resolve("static/docs/page.txt"), "page\n");
      DevelopmentRunFiles.write(directory, classFolder);
      Files.writeString(root.resolve(CURRENT_FILE), id);

      AssetResponse response =
          DeployedAssets.read(root.toString(), AssetRequest.create(APP_NAME, DOCS_FOLDER).build());

      assertEquals(404, response.getStatus());
      assertEquals(0, response.getBody().length);
    }

    @Test
    void shouldApplyTheConfiguredCacheControlAndContentType() throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
      Files.writeString(app.resolve(CONF_PATH), """
          webforj.entry = "org.example.Application"
          webforj.assetsCacheControl = "public, max-age=60"
          webforj.mime.extensions { css = "text/x-css" }
          """);
      Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
      Path root = temporary.resolve(DEPLOYMENTS_FOLDER);
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive, Files.createDirectories(root.resolve(APP_NAME)).resolve(id));
      Files.writeString(root.resolve(CURRENT_FILE), id);

      AssetResponse response =
          DeployedAssets.read(root.toString(), AssetRequest.create(APP_NAME, APP_CSS).build());

      assertEquals("public, max-age=60", response.getCacheControl());
      assertEquals("text/x-css", response.getContentType());
    }

    @ParameterizedTest
    @MethodSource("getFolderPaths")
    void shouldServeTheIndexOfFolder(String path) throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
      Files.createDirectories(app.resolve("src/main/resources/static/docs"));
      Files.writeString(app.resolve("src/main/resources/static/docs/index.html"), "<p>index</p>\n");
      Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
      Path root = temporary.resolve(DEPLOYMENTS_FOLDER);
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive, Files.createDirectories(root.resolve(APP_NAME)).resolve(id));
      Files.writeString(root.resolve(CURRENT_FILE), id);

      AssetResponse response =
          DeployedAssets.read(root.toString(), AssetRequest.create(APP_NAME, path).build());

      assertEquals(200, response.getStatus());
      assertEquals("text/html", response.getContentType());
      assertArrayEquals("<p>index</p>\n".getBytes(StandardCharsets.UTF_8), response.getBody());
    }

    @ParameterizedTest
    @MethodSource("getPagePaths")
    void shouldServeThePageNamedByTheConfiguredIndexAndExtension(String path) throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
      Files.writeString(app.resolve(CONF_PATH), """
          webforj.entry = "org.example.Application"
          webforj.assetsIndex = ["page.html"]
          webforj.assetsExt = ".html"
          """);
      Files.createDirectories(app.resolve("src/main/resources/static/docs"));
      Files.writeString(app.resolve("src/main/resources/static/docs/page.html"), "<p>page</p>\n");
      Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
      Path root = temporary.resolve(DEPLOYMENTS_FOLDER);
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive, Files.createDirectories(root.resolve(APP_NAME)).resolve(id));
      Files.writeString(root.resolve(CURRENT_FILE), id);

      AssetResponse response =
          DeployedAssets.read(root.toString(), AssetRequest.create(APP_NAME, path).build());

      assertArrayEquals("<p>page</p>\n".getBytes(StandardCharsets.UTF_8), response.getBody());
    }

    @Test
    void shouldServeEmptyFileWithItsContentType() throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
      Path generated = app.resolve("src/main/resources/static/frontend/generated");
      Files.createDirectories(generated);
      Files.write(generated.resolve("bundle.js"), new byte[0]);
      Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
      Path root = temporary.resolve(DEPLOYMENTS_FOLDER);
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive, Files.createDirectories(root.resolve(APP_NAME)).resolve(id));
      Files.writeString(root.resolve(CURRENT_FILE), id);

      AssetResponse response = DeployedAssets.read(root.toString(),
          AssetRequest.create(APP_NAME, "static/frontend/generated/bundle.js").build());

      assertEquals(200, response.getStatus());
      assertEquals("text/javascript", response.getContentType());
      assertArrayEquals(new byte[0], response.getBody());
    }

    @Test
    void shouldServeTheApplicationFileOverTheDependencyFileOfTheSameName() throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
      Files.createDirectories(app.resolve("src/main/resources/static/webforj/icon-badge"));
      Files.writeString(app.resolve("src/main/resources/static/webforj/icon-badge/icon-badge.js"),
          "console.log('hello-app');\n");
      Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
      Path root = temporary.resolve(DEPLOYMENTS_FOLDER);
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive, Files.createDirectories(root.resolve(APP_NAME)).resolve(id));
      Files.writeString(root.resolve(CURRENT_FILE), id);
      byte[] foundation;
      try (InputStream input = getClass().getClassLoader()
          .getResourceAsStream("static/webforj/icon-badge/icon-badge.min.js")) {
        foundation = input.readAllBytes();
      }

      AssetResponse shadowed = DeployedAssets.read(root.toString(),
          AssetRequest.create(APP_NAME, "static/webforj/icon-badge/icon-badge.js").build());
      AssetResponse dependency = DeployedAssets.read(root.toString(),
          AssetRequest.create(APP_NAME, "static/webforj/icon-badge/icon-badge.min.js").build());

      assertArrayEquals("console.log('hello-app');\n".getBytes(StandardCharsets.UTF_8),
          shadowed.getBody());
      assertArrayEquals(foundation, dependency.getBody());
    }

    @Test
    void shouldServeStaticFilesUnderTheConfiguredAssetsDirectoryOnly() throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
      Files.writeString(app.resolve(CONF_PATH), """
          webforj.entry = "org.example.Application"
          webforj.assetsDir = "/assets"
          """);
      Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
      Path root = temporary.resolve(DEPLOYMENTS_FOLDER);
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive, Files.createDirectories(root.resolve(APP_NAME)).resolve(id));
      Files.writeString(root.resolve(CURRENT_FILE), id);

      AssetResponse configured = DeployedAssets.read(root.toString(),
          AssetRequest.create(APP_NAME, "assets/app.css").build());
      AssetResponse standard =
          DeployedAssets.read(root.toString(), AssetRequest.create(APP_NAME, APP_CSS).build());

      assertEquals(200, configured.getStatus());
      assertEquals(404, standard.getStatus());
    }

    @Test
    void shouldServeIconsAndStaticFilesAtTheRootWhenTheAssetsDirectoryIsTheRoot()
        throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
      Files.writeString(app.resolve(CONF_PATH), """
          webforj.entry = "org.example.Application"
          webforj.assetsDir = "/"
          """);
      Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
      Path root = temporary.resolve(DEPLOYMENTS_FOLDER);
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive, Files.createDirectories(root.resolve(APP_NAME)).resolve(id));
      Files.writeString(root.resolve(CURRENT_FILE), id);

      AssetResponse icon = DeployedAssets.read(root.toString(),
          AssetRequest.create(APP_NAME, "icons/icon-144x144.png").build());
      AssetResponse file =
          DeployedAssets.read(root.toString(), AssetRequest.create(APP_NAME, "app.css").build());

      assertEquals(200, icon.getStatus());
      assertEquals(PNG_CONTENT_TYPE, icon.getContentType());
      assertEquals(200, file.getStatus());
      assertArrayEquals(Files.readAllBytes(HelloApp.getFile(APP_CSS)), file.getBody());
    }

    @ParameterizedTest
    @MethodSource("getMissingFiles")
    void shouldAnswerNotFoundForUnknownApplicationsPrefixesAndFiles(String name, String path)
        throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
      Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
      Path root = temporary.resolve(DEPLOYMENTS_FOLDER);
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive, Files.createDirectories(root.resolve(APP_NAME)).resolve(id));
      Files.writeString(root.resolve(CURRENT_FILE), id);

      AssetResponse response =
          DeployedAssets.read(root.toString(), AssetRequest.create(name, path).build());

      assertEquals(404, response.getStatus());
      assertNull(response.getContentType());
      assertEquals(0, response.getBody().length);
    }

    @Test
    void shouldIgnoreMarkerThatNamesNoDeploymentId() throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
      Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
      Path root = temporary.resolve(DEPLOYMENTS_FOLDER);
      DeploymentArchive.read(archive,
          Files.createDirectories(root.resolve(APP_NAME)).resolve("live"));
      Files.writeString(root.resolve(CURRENT_FILE), "live");

      AssetResponse response =
          DeployedAssets.read(root.toString(), AssetRequest.create(APP_NAME, APP_CSS).build());

      assertEquals(404, response.getStatus());
    }

    @Test
    void shouldServeTheDeploymentTheMarkerNamesWhileReplacementIsUnpacked() throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
      Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
      Path root = temporary.resolve(DEPLOYMENTS_FOLDER);
      String previous = UUID.randomUUID().toString();
      DeploymentArchive.read(archive,
          Files.createDirectories(root.resolve(APP_NAME)).resolve(previous));
      Files.writeString(root.resolve(CURRENT_FILE), previous);
      Files.writeString(app.resolve(APP_CSS_PATH), APP_CSS_SOURCE);
      HelloApp.writeArchive(app, archive);
      String replacement = UUID.randomUUID().toString();
      DeploymentArchive.read(archive, root.resolve(APP_NAME).resolve(replacement));
      AssetRequest request = AssetRequest.create(APP_NAME, APP_CSS).build();

      AssetResponse unpacked = DeployedAssets.read(root.toString(), request);
      Files.writeString(root.resolve(CURRENT_FILE), replacement);
      AssetResponse switched = DeployedAssets.read(root.toString(), request);

      assertArrayEquals(Files.readAllBytes(HelloApp.getFile(APP_CSS)), unpacked.getBody());
      assertArrayEquals(APP_CSS_SOURCE.getBytes(StandardCharsets.UTF_8), switched.getBody());
    }

    @Test
    void shouldServeTheReplacementOnceTheMarkerNamesAnother() throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
      Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
      Path root = temporary.resolve(DEPLOYMENTS_FOLDER);
      String previous = UUID.randomUUID().toString();
      DeploymentArchive.read(archive,
          Files.createDirectories(root.resolve(APP_NAME)).resolve(previous));
      Files.writeString(root.resolve(CURRENT_FILE), previous);
      AssetRequest request = AssetRequest.create(APP_NAME, APP_CSS).build();
      final AssetResponse replaced = DeployedAssets.read(root.toString(), request);
      Files.writeString(app.resolve(APP_CSS_PATH), APP_CSS_SOURCE);
      HelloApp.writeArchive(app, archive);
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive, root.resolve(APP_NAME).resolve(id));
      Files.writeString(root.resolve(CURRENT_FILE), id);

      AssetResponse response = DeployedAssets.read(root.toString(), request);

      assertArrayEquals(Files.readAllBytes(HelloApp.getFile(APP_CSS)), replaced.getBody());
      assertArrayEquals(APP_CSS_SOURCE.getBytes(StandardCharsets.UTF_8), response.getBody());
    }

    @Test
    void shouldAnswerNotFoundForDeploymentWithoutMarker() throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
      Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
      Path root = temporary.resolve(DEPLOYMENTS_FOLDER);
      DeploymentArchive.read(archive,
          Files.createDirectories(root.resolve(APP_NAME)).resolve(UUID.randomUUID().toString()));

      AssetResponse response =
          DeployedAssets.read(root.toString(), AssetRequest.create(APP_NAME, APP_CSS).build());

      assertEquals(404, response.getStatus());
    }

    @ParameterizedTest
    @MethodSource("getTraversalNames")
    void shouldRefuseNameThatLeavesTheDeploymentRootWithoutReadingIt(String name)
        throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
      Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
      Path root = temporary.resolve("deployments/inner");
      DeploymentArchive.read(archive, Files.createDirectories(temporary.resolve(DEPLOYMENTS_FOLDER))
          .resolve(UUID.randomUUID().toString()));

      AssetResponse response =
          DeployedAssets.read(root.toString(), AssetRequest.create(name, APP_CSS).build());

      assertEquals(404, response.getStatus());
      assertFalse(Files.exists(root));
    }

    @Test
    void shouldServeTheReplacementAfterThePreviousDeploymentFolderIsRemoved() throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
      Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
      Path root = temporary.resolve(DEPLOYMENTS_FOLDER);
      Path previous =
          Files.createDirectories(root.resolve(APP_NAME)).resolve(UUID.randomUUID().toString());
      DeploymentArchive.read(archive, previous);
      Files.writeString(root.resolve(CURRENT_FILE), previous.getFileName().toString());
      AssetRequest request = AssetRequest.create(APP_NAME, APP_CSS).build();
      DeployedAssets.read(root.toString(), request);
      try (Stream<Path> files = Files.walk(previous)) {
        for (Path file : files.sorted(Comparator.reverseOrder()).toList()) {
          Files.delete(file);
        }
      }

      Files.writeString(app.resolve(APP_CSS_PATH), APP_CSS_SOURCE);
      HelloApp.writeArchive(app, archive);
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive, Files.createDirectories(root.resolve(APP_NAME)).resolve(id));
      Files.writeString(root.resolve(CURRENT_FILE), id);

      AssetResponse response = DeployedAssets.read(root.toString(), request);

      assertArrayEquals(APP_CSS_SOURCE.getBytes(StandardCharsets.UTF_8), response.getBody());
    }

    private static Stream<String> getFolderPaths() {
      return Stream.of("static/docs/", DOCS_FOLDER);
    }

    private static Stream<String> getPagePaths() {
      return Stream.of("static/docs/", "static/docs/page");
    }

    private static Stream<String> getTraversalNames() {
      return Stream.of("..", "../inner", ".", "con", "");
    }

    private static Stream<Arguments> getMissingFiles() {
      return Stream.of(Arguments.of("other", APP_CSS), Arguments.of(APP_NAME, "static/missing.css"),
          Arguments.of(APP_NAME, "app.css"), Arguments.of(APP_NAME, "static/../webforj.conf"));
    }
  }

  @Nested
  class Icons {

    @TempDir
    Path temporary;

    @Test
    void shouldServeThePackagedIconAsItIsWithAnEntityTag() throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
      Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
      Path root = temporary.resolve(DEPLOYMENTS_FOLDER);
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive, Files.createDirectories(root.resolve(APP_NAME)).resolve(id));
      Files.writeString(root.resolve(CURRENT_FILE), id);

      AssetResponse response =
          DeployedAssets.read(root.toString(), AssetRequest.create(APP_NAME, ICON).build());

      assertEquals(200, response.getStatus());
      assertEquals(PNG_CONTENT_TYPE, response.getContentType());
      assertArrayEquals(Files.readAllBytes(HelloApp.getFile(ICON)), response.getBody());
      assertTrue(response.getEntityTag().matches("\"[A-Za-z0-9_=-]+\""));
    }

    @ParameterizedTest
    @MethodSource("getServedIcons")
    void shouldAnswerNotModifiedForTheEntityTagOfTheServedIcon(String path) throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
      Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
      Path root = temporary.resolve(DEPLOYMENTS_FOLDER);
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive, Files.createDirectories(root.resolve(APP_NAME)).resolve(id));
      Files.writeString(root.resolve(CURRENT_FILE), id);
      String entityTag = DeployedAssets
          .read(root.toString(), AssetRequest.create(APP_NAME, path).build()).getEntityTag();

      AssetResponse response = DeployedAssets.read(root.toString(),
          AssetRequest.create(APP_NAME, path).setIfNoneMatch(entityTag).build());

      assertEquals(304, response.getStatus());
      assertEquals(entityTag, response.getEntityTag());
      assertEquals(0, response.getBody().length);
    }

    @Test
    void shouldScaleThePackagedIconToTheRequestedSize() throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
      Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
      Path root = temporary.resolve(DEPLOYMENTS_FOLDER);
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive, Files.createDirectories(root.resolve(APP_NAME)).resolve(id));
      Files.writeString(root.resolve(CURRENT_FILE), id);

      AssetResponse response = DeployedAssets.read(root.toString(),
          AssetRequest.create(APP_NAME, "icons/icon-32x24.png").build());

      BufferedImage icon = ImageIO.read(new ByteArrayInputStream(response.getBody()));
      assertEquals(200, response.getStatus());
      assertEquals(PNG_CONTENT_TYPE, response.getContentType());
      assertEquals(32, icon.getWidth());
      assertEquals(24, icon.getHeight());
      assertEquals(0, new Color(icon.getRGB(0, 0), true).getAlpha());
      assertEquals(255, new Color(icon.getRGB(16, 12), true).getAlpha());
    }

    @Test
    void shouldFillThePaddingOfTheScaledIconWithTheRequestedBackground() throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
      Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
      Path root = temporary.resolve(DEPLOYMENTS_FOLDER);
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive, Files.createDirectories(root.resolve(APP_NAME)).resolve(id));
      Files.writeString(root.resolve(CURRENT_FILE), id);

      AssetResponse response = DeployedAssets.read(root.toString(), AssetRequest
          .create(APP_NAME, SIZED_ICON).setPadding(".5").setBackground("00ff00").build());

      BufferedImage icon = ImageIO.read(new ByteArrayInputStream(response.getBody()));
      assertEquals(Color.GREEN.getRGB(), icon.getRGB(1, 1));
      assertNotEquals(Color.GREEN.getRGB(), icon.getRGB(10, 10));
    }

    @ParameterizedTest
    @MethodSource("getDifferentIconRequests")
    void shouldDeriveDistinctEntityTagForEachIconParameter(AssetRequest first, AssetRequest second)
        throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
      Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
      Path root = temporary.resolve(DEPLOYMENTS_FOLDER);
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive, Files.createDirectories(root.resolve(APP_NAME)).resolve(id));
      Files.writeString(root.resolve(CURRENT_FILE), id);

      String firstTag = DeployedAssets.read(root.toString(), first).getEntityTag();
      String secondTag = DeployedAssets.read(root.toString(), second).getEntityTag();

      assertNotEquals(firstTag, secondTag);
    }

    @ParameterizedTest
    @MethodSource("getIconsWithoutBase")
    void shouldScaleTheDefaultIconWhenTheBaseIconIsMissing(String path, String contentType,
        int size) throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
      Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
      Path root = temporary.resolve(DEPLOYMENTS_FOLDER);
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive, Files.createDirectories(root.resolve(APP_NAME)).resolve(id));
      Files.writeString(root.resolve(CURRENT_FILE), id);

      AssetResponse response =
          DeployedAssets.read(root.toString(), AssetRequest.create(APP_NAME, path).build());

      BufferedImage icon = ImageIO.read(new ByteArrayInputStream(response.getBody()));
      assertEquals(200, response.getStatus());
      assertEquals(contentType, response.getContentType());
      assertEquals(size, icon.getWidth());
      assertEquals(size, icon.getHeight());
      assertNotNull(response.getEntityTag());
    }

    @Test
    void shouldServeTheDefaultIconForMissingIconWithoutSize() throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
      Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
      Path root = temporary.resolve(DEPLOYMENTS_FOLDER);
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive, Files.createDirectories(root.resolve(APP_NAME)).resolve(id));
      Files.writeString(root.resolve(CURRENT_FILE), id);

      AssetResponse response = DeployedAssets.read(root.toString(),
          AssetRequest.create(APP_NAME, "icons/missing.png").build());

      assertEquals(200, response.getStatus());
      assertNotNull(ImageIO.read(new ByteArrayInputStream(response.getBody())));
    }

    @ParameterizedTest
    @MethodSource("getRejectedIcons")
    void shouldAnswerNotFoundForNonImagePathsUnknownApplicationsAndMissingNonPngIcons(String name,
        String path) throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
      Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
      Path root = temporary.resolve(DEPLOYMENTS_FOLDER);
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive, Files.createDirectories(root.resolve(APP_NAME)).resolve(id));
      Files.writeString(root.resolve(CURRENT_FILE), id);

      AssetResponse response =
          DeployedAssets.read(root.toString(), AssetRequest.create(name, path).build());

      assertEquals(404, response.getStatus());
      assertNull(response.getEntityTag());
    }

    @ParameterizedTest
    @MethodSource("getOutOfBoundsIcons")
    void shouldAnswerNotFoundForIconSizeOutOfBoundsWithoutDerivingIt(String path)
        throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
      Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
      Path root = temporary.resolve(DEPLOYMENTS_FOLDER);
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive, Files.createDirectories(root.resolve(APP_NAME)).resolve(id));
      Files.writeString(root.resolve(CURRENT_FILE), id);

      AssetResponse response =
          DeployedAssets.read(root.toString(), AssetRequest.create(APP_NAME, path).build());

      assertEquals(404, response.getStatus());
      assertNull(response.getEntityTag());
    }

    @Test
    void shouldScaleThePackagedIconToTheLargestAllowedSize() throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
      Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
      Path root = temporary.resolve(DEPLOYMENTS_FOLDER);
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive, Files.createDirectories(root.resolve(APP_NAME)).resolve(id));
      Files.writeString(root.resolve(CURRENT_FILE), id);

      AssetResponse response = DeployedAssets.read(root.toString(),
          AssetRequest.create(APP_NAME, "icons/icon-1024x1024.png").build());

      BufferedImage icon = ImageIO.read(new ByteArrayInputStream(response.getBody()));
      assertEquals(200, response.getStatus());
      assertEquals(1024, icon.getWidth());
      assertEquals(1024, icon.getHeight());
    }

    @Test
    void shouldServeIconsUnderTheConfiguredIconsDirectoryOnly() throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
      Files.writeString(app.resolve(CONF_PATH), """
          webforj.entry = "org.example.Application"
          webforj.iconsDir = "brand/"
          """);
      Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_NAME));
      Path root = temporary.resolve(DEPLOYMENTS_FOLDER);
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive, Files.createDirectories(root.resolve(APP_NAME)).resolve(id));
      Files.writeString(root.resolve(CURRENT_FILE), id);

      AssetResponse configured = DeployedAssets.read(root.toString(),
          AssetRequest.create(APP_NAME, "brand/icon.png").build());
      AssetResponse standard =
          DeployedAssets.read(root.toString(), AssetRequest.create(APP_NAME, ICON).build());

      assertEquals(200, configured.getStatus());
      assertEquals(404, standard.getStatus());
    }

    private static Stream<String> getServedIcons() {
      return Stream.of(ICON, SIZED_ICON);
    }

    private static Stream<Arguments> getDifferentIconRequests() {
      AssetRequest plain = AssetRequest.create(APP_NAME, SIZED_ICON).build();
      return Stream.of(
          Arguments.of(plain, AssetRequest.create(APP_NAME, "icons/icon-21x20.png").build()),
          Arguments.of(plain, AssetRequest.create(APP_NAME, SIZED_ICON).setPadding(".5").build()),
          Arguments.of(plain,
              AssetRequest.create(APP_NAME, SIZED_ICON).setBackground("00ff00").build()),
          Arguments.of(AssetRequest.create(APP_NAME, "icons/other-20x20.png").build(),
              AssetRequest.create(APP_NAME, "icons/other-20x20.gif").build()));
    }

    private static Stream<Arguments> getIconsWithoutBase() {
      return Stream.of(Arguments.of("icons/other-24x24.png", PNG_CONTENT_TYPE, 24),
          Arguments.of("icons/other-16x16.gif", "image/gif", 16));
    }

    private static Stream<Arguments> getRejectedIcons() {
      return Stream.of(Arguments.of(APP_NAME, "icons/icon-16x16.txt"),
          Arguments.of(APP_NAME, "icons/icon-16x16"), Arguments.of("other", ICON),
          Arguments.of(APP_NAME, "icons/missing.jpeg"));
    }

    private static Stream<String> getOutOfBoundsIcons() {
      return Stream.of("icons/icon-20000x20000.png", "icons/icon-1025x16.png",
          "icons/icon-0x16.png", "icons/icon-99999999999x16.png");
    }
  }
}
