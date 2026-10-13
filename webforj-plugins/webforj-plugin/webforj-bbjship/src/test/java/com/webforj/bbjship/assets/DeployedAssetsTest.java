package com.webforj.bbjship.assets;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.bbjship.assets.model.AssetRequest;
import com.webforj.bbjship.assets.model.AssetResponse;
import com.webforj.plugin.foundation.bbj.DeploymentArchive;
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
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Stream;
import javax.imageio.ImageIO;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

final class DeployedAssetsTest {

  @Nested
  class StaticFiles {

    @TempDir
    Path temporary;

    @Test
    void shouldCloseTheCachedApplicationOfTheFolderSoItCanBeRemoved() throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
      Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
      Path root = temporary.resolve("deployments");
      String id = UUID.randomUUID().toString();
      Path deployment = Files.createDirectories(root.resolve("hello-app")).resolve(id);
      DeploymentArchive.read(archive, deployment);
      Files.writeString(root.resolve("hello-app/current"), id);
      DeployedApplication cached = DeployedAssets.findApplication(root, "hello-app");
      assertNotNull(cached);

      DeployedAssets.close(deployment);

      assertEquals(Optional.empty(), cached.readResource("static/app.css"));
      DeployedApplication reloaded = DeployedAssets.findApplication(root, "hello-app");
      assertNotSame(cached, reloaded);
      assertTrue(reloaded.readResource("static/app.css").isPresent());
      DeployedAssets.close(deployment);
    }

    @Test
    void shouldServeTheStylesheetOfTheApplicationWithoutCacheControl() throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
      Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
      Path root = temporary.resolve("deployments");
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive,
          Files.createDirectories(root.resolve("hello-app")).resolve(id));
      Files.writeString(root.resolve("hello-app/current"), id);

      AssetResponse response = DeployedAssets.read(root.toString(),
          AssetRequest.create("hello-app", "static/app.css").build());

      assertEquals(200, response.getStatus());
      assertEquals("text/css", response.getContentType());
      assertNull(response.getCacheControl());
      assertNull(response.getEntityTag());
      assertArrayEquals(Files.readAllBytes(HelloApp.getFile("static/app.css")), response.getBody());
    }

    @Test
    void shouldServeTheStylesheetOfTheClassFolderOfDevelopmentRun() throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
      Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
      Path root = temporary.resolve("deployments");
      String id = UUID.randomUUID().toString();
      Path directory = Files.createDirectories(root.resolve("hello-app")).resolve(id);
      DeploymentArchive.read(archive, directory);
      Files.delete(directory.resolve("application.jar"));
      Path classFolder = app.resolve("src/main/resources");
      Files.writeString(classFolder.resolve("static/app.css"), "body { color: red; }\n");
      Files.writeString(directory.resolve("development"), classFolder.toString());
      Files.writeString(root.resolve("hello-app/current"), id);

      AssetResponse response = DeployedAssets.read(root.toString(),
          AssetRequest.create("hello-app", "static/app.css").build());

      assertEquals(200, response.getStatus());
      assertEquals("text/css", response.getContentType());
      assertArrayEquals("body { color: red; }\n".getBytes(StandardCharsets.UTF_8),
          response.getBody());
    }

    @Test
    void shouldAnswerNotFoundForFolderWithoutIndexOfTheClassFolderOfDevelopmentRun()
        throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
      Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
      Path root = temporary.resolve("deployments");
      String id = UUID.randomUUID().toString();
      Path directory = Files.createDirectories(root.resolve("hello-app")).resolve(id);
      DeploymentArchive.read(archive, directory);
      Files.delete(directory.resolve("application.jar"));
      Path classFolder = app.resolve("src/main/resources");
      Files.createDirectories(classFolder.resolve("static/docs"));
      Files.writeString(classFolder.resolve("static/docs/page.txt"), "page\n");
      Files.writeString(directory.resolve("development"), classFolder.toString());
      Files.writeString(root.resolve("hello-app/current"), id);

      AssetResponse response = DeployedAssets.read(root.toString(),
          AssetRequest.create("hello-app", "static/docs").build());

      assertEquals(404, response.getStatus());
      assertEquals(0, response.getBody().length);
    }

    @Test
    void shouldApplyTheConfiguredCacheControlAndContentType() throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
      Files.writeString(app.resolve("src/main/resources/webforj.conf"), """
          webforj.entry = "org.example.Application"
          webforj.assetsCacheControl = "public, max-age=60"
          webforj.mime.extensions { css = "text/x-css" }
          """);
      Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
      Path root = temporary.resolve("deployments");
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive,
          Files.createDirectories(root.resolve("hello-app")).resolve(id));
      Files.writeString(root.resolve("hello-app/current"), id);

      AssetResponse response = DeployedAssets.read(root.toString(),
          AssetRequest.create("hello-app", "static/app.css").build());

      assertEquals("public, max-age=60", response.getCacheControl());
      assertEquals("text/x-css", response.getContentType());
    }

    @ParameterizedTest
    @MethodSource("getFolderPaths")
    void shouldServeTheIndexOfFolder(String path) throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
      Files.createDirectories(app.resolve("src/main/resources/static/docs"));
      Files.writeString(app.resolve("src/main/resources/static/docs/index.html"), "<p>index</p>\n");
      Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
      Path root = temporary.resolve("deployments");
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive,
          Files.createDirectories(root.resolve("hello-app")).resolve(id));
      Files.writeString(root.resolve("hello-app/current"), id);

      AssetResponse response =
          DeployedAssets.read(root.toString(), AssetRequest.create("hello-app", path).build());

      assertEquals(200, response.getStatus());
      assertEquals("text/html", response.getContentType());
      assertArrayEquals("<p>index</p>\n".getBytes(StandardCharsets.UTF_8), response.getBody());
    }

    @ParameterizedTest
    @MethodSource("getPagePaths")
    void shouldServeThePageNamedByTheConfiguredIndexAndExtension(String path) throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
      Files.writeString(app.resolve("src/main/resources/webforj.conf"), """
          webforj.entry = "org.example.Application"
          webforj.assetsIndex = ["page.html"]
          webforj.assetsExt = ".html"
          """);
      Files.createDirectories(app.resolve("src/main/resources/static/docs"));
      Files.writeString(app.resolve("src/main/resources/static/docs/page.html"), "<p>page</p>\n");
      Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
      Path root = temporary.resolve("deployments");
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive,
          Files.createDirectories(root.resolve("hello-app")).resolve(id));
      Files.writeString(root.resolve("hello-app/current"), id);

      AssetResponse response =
          DeployedAssets.read(root.toString(), AssetRequest.create("hello-app", path).build());

      assertArrayEquals("<p>page</p>\n".getBytes(StandardCharsets.UTF_8), response.getBody());
    }

    @Test
    void shouldServeEmptyFileWithItsContentType() throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
      Path generated = app.resolve("src/main/resources/static/frontend/generated");
      Files.createDirectories(generated);
      Files.write(generated.resolve("bundle.js"), new byte[0]);
      Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
      Path root = temporary.resolve("deployments");
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive,
          Files.createDirectories(root.resolve("hello-app")).resolve(id));
      Files.writeString(root.resolve("hello-app/current"), id);

      AssetResponse response = DeployedAssets.read(root.toString(),
          AssetRequest.create("hello-app", "static/frontend/generated/bundle.js").build());

      assertEquals(200, response.getStatus());
      assertEquals("text/javascript", response.getContentType());
      assertArrayEquals(new byte[0], response.getBody());
    }

    @Test
    void shouldServeTheApplicationFileOverTheDependencyFileOfTheSameName() throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
      Files.createDirectories(app.resolve("src/main/resources/static/webforj/icon-badge"));
      Files.writeString(app.resolve("src/main/resources/static/webforj/icon-badge/icon-badge.js"),
          "console.log('hello-app');\n");
      Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
      Path root = temporary.resolve("deployments");
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive,
          Files.createDirectories(root.resolve("hello-app")).resolve(id));
      Files.writeString(root.resolve("hello-app/current"), id);
      byte[] foundation;
      try (InputStream input = getClass().getClassLoader()
          .getResourceAsStream("static/webforj/icon-badge/icon-badge.min.js")) {
        foundation = input.readAllBytes();
      }

      AssetResponse shadowed = DeployedAssets.read(root.toString(),
          AssetRequest.create("hello-app", "static/webforj/icon-badge/icon-badge.js").build());
      AssetResponse dependency = DeployedAssets.read(root.toString(),
          AssetRequest.create("hello-app", "static/webforj/icon-badge/icon-badge.min.js").build());

      assertArrayEquals("console.log('hello-app');\n".getBytes(StandardCharsets.UTF_8),
          shadowed.getBody());
      assertArrayEquals(foundation, dependency.getBody());
    }

    @Test
    void shouldServeStaticFilesUnderTheConfiguredAssetsDirectoryOnly() throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
      Files.writeString(app.resolve("src/main/resources/webforj.conf"), """
          webforj.entry = "org.example.Application"
          webforj.assetsDir = "/assets"
          """);
      Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
      Path root = temporary.resolve("deployments");
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive,
          Files.createDirectories(root.resolve("hello-app")).resolve(id));
      Files.writeString(root.resolve("hello-app/current"), id);

      AssetResponse configured = DeployedAssets.read(root.toString(),
          AssetRequest.create("hello-app", "assets/app.css").build());
      AssetResponse standard = DeployedAssets.read(root.toString(),
          AssetRequest.create("hello-app", "static/app.css").build());

      assertEquals(200, configured.getStatus());
      assertEquals(404, standard.getStatus());
    }

    @Test
    void shouldServeIconsAndStaticFilesAtTheRootWhenTheAssetsDirectoryIsTheRoot()
        throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
      Files.writeString(app.resolve("src/main/resources/webforj.conf"), """
          webforj.entry = "org.example.Application"
          webforj.assetsDir = "/"
          """);
      Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
      Path root = temporary.resolve("deployments");
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive,
          Files.createDirectories(root.resolve("hello-app")).resolve(id));
      Files.writeString(root.resolve("hello-app/current"), id);

      AssetResponse icon = DeployedAssets.read(root.toString(),
          AssetRequest.create("hello-app", "icons/icon-144x144.png").build());
      AssetResponse file =
          DeployedAssets.read(root.toString(), AssetRequest.create("hello-app", "app.css").build());

      assertEquals(200, icon.getStatus());
      assertEquals("image/png", icon.getContentType());
      assertEquals(200, file.getStatus());
      assertArrayEquals(Files.readAllBytes(HelloApp.getFile("static/app.css")), file.getBody());
    }

    @ParameterizedTest
    @MethodSource("getMissingFiles")
    void shouldAnswerNotFoundForUnknownApplicationsPrefixesAndFiles(String name, String path)
        throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
      Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
      Path root = temporary.resolve("deployments");
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive,
          Files.createDirectories(root.resolve("hello-app")).resolve(id));
      Files.writeString(root.resolve("hello-app/current"), id);

      AssetResponse response =
          DeployedAssets.read(root.toString(), AssetRequest.create(name, path).build());

      assertEquals(404, response.getStatus());
      assertNull(response.getContentType());
      assertEquals(0, response.getBody().length);
    }

    @Test
    void shouldIgnoreMarkerThatNamesNoDeploymentId() throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
      Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
      Path root = temporary.resolve("deployments");
      DeploymentArchive.read(archive,
          Files.createDirectories(root.resolve("hello-app")).resolve("live"));
      Files.writeString(root.resolve("hello-app/current"), "live");

      AssetResponse response = DeployedAssets.read(root.toString(),
          AssetRequest.create("hello-app", "static/app.css").build());

      assertEquals(404, response.getStatus());
    }

    @Test
    void shouldServeTheDeploymentTheMarkerNamesWhileReplacementIsUnpacked() throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
      Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
      Path root = temporary.resolve("deployments");
      String previous = UUID.randomUUID().toString();
      DeploymentArchive.read(archive,
          Files.createDirectories(root.resolve("hello-app")).resolve(previous));
      Files.writeString(root.resolve("hello-app/current"), previous);
      Files.writeString(app.resolve("src/main/resources/static/app.css"), "body {}\n");
      HelloApp.writeArchive(app, archive);
      String replacement = UUID.randomUUID().toString();
      DeploymentArchive.read(archive, root.resolve("hello-app").resolve(replacement));
      AssetRequest request = AssetRequest.create("hello-app", "static/app.css").build();

      AssetResponse unpacked = DeployedAssets.read(root.toString(), request);
      Files.writeString(root.resolve("hello-app/current"), replacement);
      AssetResponse switched = DeployedAssets.read(root.toString(), request);

      assertArrayEquals(Files.readAllBytes(HelloApp.getFile("static/app.css")), unpacked.getBody());
      assertArrayEquals("body {}\n".getBytes(StandardCharsets.UTF_8), switched.getBody());
    }

    @Test
    void shouldCloseTheReplacedDeploymentOnceTheMarkerNamesAnother() throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
      Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
      Path root = temporary.resolve("deployments");
      String previous = UUID.randomUUID().toString();
      DeploymentArchive.read(archive,
          Files.createDirectories(root.resolve("hello-app")).resolve(previous));
      Files.writeString(root.resolve("hello-app/current"), previous);
      final DeployedApplication replaced = DeployedAssets.findApplication(root, "hello-app");
      Files.writeString(app.resolve("src/main/resources/static/app.css"), "body {}\n");
      HelloApp.writeArchive(app, archive);
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive, root.resolve("hello-app").resolve(id));
      Files.writeString(root.resolve("hello-app/current"), id);

      AssetResponse response = DeployedAssets.read(root.toString(),
          AssetRequest.create("hello-app", "static/app.css").build());

      assertArrayEquals("body {}\n".getBytes(StandardCharsets.UTF_8), response.getBody());
      assertEquals(Optional.empty(), replaced.readResource("static/app.css"));
    }

    @Test
    void shouldAnswerNotFoundForDeploymentWithoutMarker() throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
      Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
      Path root = temporary.resolve("deployments");
      DeploymentArchive.read(archive,
          Files.createDirectories(root.resolve("hello-app")).resolve(UUID.randomUUID().toString()));

      AssetResponse response = DeployedAssets.read(root.toString(),
          AssetRequest.create("hello-app", "static/app.css").build());

      assertEquals(404, response.getStatus());
    }

    @ParameterizedTest
    @MethodSource("getTraversalNames")
    void shouldRefuseNameThatLeavesTheDeploymentRootWithoutReadingIt(String name)
        throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
      Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
      Path root = temporary.resolve("deployments/inner");
      DeploymentArchive.read(archive, Files.createDirectories(temporary.resolve("deployments"))
          .resolve(UUID.randomUUID().toString()));

      AssetResponse response =
          DeployedAssets.read(root.toString(), AssetRequest.create(name, "static/app.css").build());

      assertEquals(404, response.getStatus());
      assertFalse(Files.exists(root));
    }

    @Test
    void shouldServeTheReplacementAfterThePreviousDeploymentFolderIsRemoved() throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
      Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
      Path root = temporary.resolve("deployments");
      Path previous =
          Files.createDirectories(root.resolve("hello-app")).resolve(UUID.randomUUID().toString());
      DeploymentArchive.read(archive, previous);
      Files.writeString(root.resolve("hello-app/current"), previous.getFileName().toString());
      AssetRequest request = AssetRequest.create("hello-app", "static/app.css").build();
      DeployedAssets.read(root.toString(), request);
      try (Stream<Path> files = Files.walk(previous)) {
        for (Path file : files.sorted(Comparator.reverseOrder()).toList()) {
          Files.delete(file);
        }
      }

      Files.writeString(app.resolve("src/main/resources/static/app.css"), "body {}\n");
      HelloApp.writeArchive(app, archive);
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive,
          Files.createDirectories(root.resolve("hello-app")).resolve(id));
      Files.writeString(root.resolve("hello-app/current"), id);

      AssetResponse response = DeployedAssets.read(root.toString(), request);

      assertArrayEquals("body {}\n".getBytes(StandardCharsets.UTF_8), response.getBody());
    }

    private static Stream<String> getFolderPaths() {
      return Stream.of("static/docs/", "static/docs");
    }

    private static Stream<String> getPagePaths() {
      return Stream.of("static/docs/", "static/docs/page");
    }

    private static Stream<String> getTraversalNames() {
      return Stream.of("..", "../inner", ".", "con", "");
    }

    private static Stream<Arguments> getMissingFiles() {
      return Stream.of(Arguments.of("other", "static/app.css"),
          Arguments.of("hello-app", "static/missing.css"), Arguments.of("hello-app", "app.css"),
          Arguments.of("hello-app", "static/../webforj.conf"));
    }
  }

  @Nested
  class Icons {

    @TempDir
    Path temporary;

    @Test
    void shouldServeThePackagedIconAsItIsWithAnEntityTag() throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
      Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
      Path root = temporary.resolve("deployments");
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive,
          Files.createDirectories(root.resolve("hello-app")).resolve(id));
      Files.writeString(root.resolve("hello-app/current"), id);

      AssetResponse response = DeployedAssets.read(root.toString(),
          AssetRequest.create("hello-app", "icons/icon.png").build());

      assertEquals(200, response.getStatus());
      assertEquals("image/png", response.getContentType());
      assertArrayEquals(Files.readAllBytes(HelloApp.getFile("icons/icon.png")), response.getBody());
      assertTrue(response.getEntityTag().matches("\"[A-Za-z0-9_=-]+\""));
    }

    @ParameterizedTest
    @MethodSource("getServedIcons")
    void shouldAnswerNotModifiedForTheEntityTagOfTheServedIcon(String path) throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
      Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
      Path root = temporary.resolve("deployments");
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive,
          Files.createDirectories(root.resolve("hello-app")).resolve(id));
      Files.writeString(root.resolve("hello-app/current"), id);
      String entityTag = DeployedAssets
          .read(root.toString(), AssetRequest.create("hello-app", path).build()).getEntityTag();

      AssetResponse response = DeployedAssets.read(root.toString(),
          AssetRequest.create("hello-app", path).setIfNoneMatch(entityTag).build());

      assertEquals(304, response.getStatus());
      assertEquals(entityTag, response.getEntityTag());
      assertEquals(0, response.getBody().length);
    }

    @Test
    void shouldScaleThePackagedIconToTheRequestedSize() throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
      Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
      Path root = temporary.resolve("deployments");
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive,
          Files.createDirectories(root.resolve("hello-app")).resolve(id));
      Files.writeString(root.resolve("hello-app/current"), id);

      AssetResponse response = DeployedAssets.read(root.toString(),
          AssetRequest.create("hello-app", "icons/icon-32x24.png").build());

      BufferedImage icon = ImageIO.read(new ByteArrayInputStream(response.getBody()));
      assertEquals(200, response.getStatus());
      assertEquals("image/png", response.getContentType());
      assertEquals(32, icon.getWidth());
      assertEquals(24, icon.getHeight());
      assertEquals(0, new Color(icon.getRGB(0, 0), true).getAlpha());
      assertEquals(255, new Color(icon.getRGB(16, 12), true).getAlpha());
    }

    @Test
    void shouldFillThePaddingOfTheScaledIconWithTheRequestedBackground() throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
      Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
      Path root = temporary.resolve("deployments");
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive,
          Files.createDirectories(root.resolve("hello-app")).resolve(id));
      Files.writeString(root.resolve("hello-app/current"), id);

      AssetResponse response = DeployedAssets.read(root.toString(),
          AssetRequest.create("hello-app", "icons/icon-20x20.png").setPadding(".5")
              .setBackground("00ff00").build());

      BufferedImage icon = ImageIO.read(new ByteArrayInputStream(response.getBody()));
      assertEquals(Color.GREEN.getRGB(), icon.getRGB(1, 1));
      assertNotEquals(Color.GREEN.getRGB(), icon.getRGB(10, 10));
    }

    @ParameterizedTest
    @MethodSource("getDifferentIconRequests")
    void shouldDeriveDistinctEntityTagForEachIconParameter(AssetRequest first, AssetRequest second)
        throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
      Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
      Path root = temporary.resolve("deployments");
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive,
          Files.createDirectories(root.resolve("hello-app")).resolve(id));
      Files.writeString(root.resolve("hello-app/current"), id);

      String firstTag = DeployedAssets.read(root.toString(), first).getEntityTag();
      String secondTag = DeployedAssets.read(root.toString(), second).getEntityTag();

      assertNotEquals(firstTag, secondTag);
    }

    @ParameterizedTest
    @MethodSource("getIconsWithoutBase")
    void shouldScaleTheDefaultIconWhenTheBaseIconIsMissing(String path, String contentType,
        int size) throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
      Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
      Path root = temporary.resolve("deployments");
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive,
          Files.createDirectories(root.resolve("hello-app")).resolve(id));
      Files.writeString(root.resolve("hello-app/current"), id);

      AssetResponse response =
          DeployedAssets.read(root.toString(), AssetRequest.create("hello-app", path).build());

      BufferedImage icon = ImageIO.read(new ByteArrayInputStream(response.getBody()));
      assertEquals(200, response.getStatus());
      assertEquals(contentType, response.getContentType());
      assertEquals(size, icon.getWidth());
      assertEquals(size, icon.getHeight());
      assertNotNull(response.getEntityTag());
    }

    @Test
    void shouldServeTheDefaultIconForMissingIconWithoutSize() throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
      Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
      Path root = temporary.resolve("deployments");
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive,
          Files.createDirectories(root.resolve("hello-app")).resolve(id));
      Files.writeString(root.resolve("hello-app/current"), id);

      AssetResponse response = DeployedAssets.read(root.toString(),
          AssetRequest.create("hello-app", "icons/missing.png").build());

      assertEquals(200, response.getStatus());
      assertNotNull(ImageIO.read(new ByteArrayInputStream(response.getBody())));
    }

    @ParameterizedTest
    @MethodSource("getRejectedIcons")
    void shouldAnswerNotFoundForNonImagePathsUnknownApplicationsAndMissingNonPngIcons(String name,
        String path) throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
      Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
      Path root = temporary.resolve("deployments");
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive,
          Files.createDirectories(root.resolve("hello-app")).resolve(id));
      Files.writeString(root.resolve("hello-app/current"), id);

      AssetResponse response =
          DeployedAssets.read(root.toString(), AssetRequest.create(name, path).build());

      assertEquals(404, response.getStatus());
      assertNull(response.getEntityTag());
    }

    @ParameterizedTest
    @MethodSource("getOutOfBoundsIcons")
    void shouldAnswerNotFoundForIconSizeOutOfBoundsWithoutDerivingIt(String path)
        throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
      Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
      Path root = temporary.resolve("deployments");
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive,
          Files.createDirectories(root.resolve("hello-app")).resolve(id));
      Files.writeString(root.resolve("hello-app/current"), id);

      AssetResponse response =
          DeployedAssets.read(root.toString(), AssetRequest.create("hello-app", path).build());

      assertEquals(404, response.getStatus());
      assertNull(response.getEntityTag());
    }

    @Test
    void shouldScaleThePackagedIconToTheLargestAllowedSize() throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
      Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
      Path root = temporary.resolve("deployments");
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive,
          Files.createDirectories(root.resolve("hello-app")).resolve(id));
      Files.writeString(root.resolve("hello-app/current"), id);

      AssetResponse response = DeployedAssets.read(root.toString(),
          AssetRequest.create("hello-app", "icons/icon-1024x1024.png").build());

      BufferedImage icon = ImageIO.read(new ByteArrayInputStream(response.getBody()));
      assertEquals(200, response.getStatus());
      assertEquals(1024, icon.getWidth());
      assertEquals(1024, icon.getHeight());
    }

    @Test
    void shouldServeIconsUnderTheConfiguredIconsDirectoryOnly() throws IOException {
      Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
      Files.writeString(app.resolve("src/main/resources/webforj.conf"), """
          webforj.entry = "org.example.Application"
          webforj.iconsDir = "brand/"
          """);
      Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
      Path root = temporary.resolve("deployments");
      String id = UUID.randomUUID().toString();
      DeploymentArchive.read(archive,
          Files.createDirectories(root.resolve("hello-app")).resolve(id));
      Files.writeString(root.resolve("hello-app/current"), id);

      AssetResponse configured = DeployedAssets.read(root.toString(),
          AssetRequest.create("hello-app", "brand/icon.png").build());
      AssetResponse standard = DeployedAssets.read(root.toString(),
          AssetRequest.create("hello-app", "icons/icon.png").build());

      assertEquals(200, configured.getStatus());
      assertEquals(404, standard.getStatus());
    }

    private static Stream<String> getServedIcons() {
      return Stream.of("icons/icon.png", "icons/icon-20x20.png");
    }

    private static Stream<Arguments> getDifferentIconRequests() {
      AssetRequest plain = AssetRequest.create("hello-app", "icons/icon-20x20.png").build();
      return Stream.of(
          Arguments.of(plain, AssetRequest.create("hello-app", "icons/icon-21x20.png").build()),
          Arguments.of(plain,
              AssetRequest.create("hello-app", "icons/icon-20x20.png").setPadding(".5").build()),
          Arguments.of(plain,
              AssetRequest.create("hello-app", "icons/icon-20x20.png").setBackground("00ff00")
                  .build()),
          Arguments.of(AssetRequest.create("hello-app", "icons/other-20x20.png").build(),
              AssetRequest.create("hello-app", "icons/other-20x20.gif").build()));
    }

    private static Stream<Arguments> getIconsWithoutBase() {
      return Stream.of(Arguments.of("icons/other-24x24.png", "image/png", 24),
          Arguments.of("icons/other-16x16.gif", "image/gif", 16));
    }

    private static Stream<Arguments> getRejectedIcons() {
      return Stream.of(Arguments.of("hello-app", "icons/icon-16x16.txt"),
          Arguments.of("hello-app", "icons/icon-16x16"), Arguments.of("other", "icons/icon.png"),
          Arguments.of("hello-app", "icons/missing.jpeg"));
    }

    private static Stream<String> getOutOfBoundsIcons() {
      return Stream.of("icons/icon-20000x20000.png", "icons/icon-1025x16.png",
          "icons/icon-0x16.png", "icons/icon-99999999999x16.png");
    }
  }
}
