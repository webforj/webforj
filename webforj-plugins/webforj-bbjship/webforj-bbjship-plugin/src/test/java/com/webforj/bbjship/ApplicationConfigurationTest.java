package com.webforj.bbjship;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.bbjship.foundation.HelloApp;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

final class ApplicationConfigurationTest {

  private static final String APP_NAME = "hello-app";
  private static final String CONF_PATH = "src/main/resources/webforj.conf";
  private static final String RESOURCES_FOLDER = "src/main/resources";
  private static final String CONF_FILE = "webforj.conf";
  private static final String ENTRY_CLASS = "org.example.Application";
  private static final String QUIET_ENTRY_CONF = """
      webforj.entry = "org.example.Application"
      webforj.quiet = true
      """;

  @TempDir
  Path temporary;

  @Test
  void shouldUseDefaultsWhenTheJarHasNoWebforjConf() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Files.delete(app.resolve(CONF_PATH));
    Path jar = HelloApp.writeJar(app);

    ApplicationConfiguration configuration = ApplicationConfiguration.load(jar);

    assertEquals(Optional.empty(), configuration.getEntryClass());
    assertEquals("static/", configuration.getAssetsPrefix());
    assertEquals("icons/", configuration.getIconsPrefix());
    assertEquals(List.of("index.html"), configuration.getIndexes());
    assertEquals(List.of(""), configuration.getExtensions());
    assertNull(configuration.getCacheControl());
    assertEquals(Map.of(), configuration.getMimeExtensions());
    assertEquals(Optional.empty(), configuration.getQuietMode());
    assertEquals(Optional.empty(), configuration.getShowBrowserWarning());
    assertEquals(Optional.empty(), configuration.getManageBrowserHistory());
    assertEquals(Optional.empty(), configuration.getSecure());
    assertFalse(configuration.isClientConfirmationDialogShown());
    assertTrue(configuration.isConsoleDisallowed());
    assertEquals(Optional.empty(), configuration.getConfigFile());
    assertEquals(List.of(), configuration.getProgramArguments());
  }

  @Test
  void shouldReadEveryKeyOfWebforjConf() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Files.writeString(app.resolve(CONF_PATH), """
        webforj {
          entry = "org.example.Application"
          assetsDir = "\\\\public"
          iconsDir = "images/"
          assetsIndex = ["home.html", "index.htm"]
          assetsExt = [".html"]
          assetsCacheControl = "max-age=60"
          mime.extensions { custom = "text/custom" }
          quiet = true
          showBrowserWarning = false
          manageBrowserHistory = true
          showClientConfirmationDialog = true
          disallowConsole = false
          configFile = "config/app.cfg"
          programArguments = ["--first", "second"]
        }
        """);
    Path jar = HelloApp.writeJar(app);

    ApplicationConfiguration configuration = ApplicationConfiguration.load(jar);

    assertEquals(Optional.of(ENTRY_CLASS), configuration.getEntryClass());
    assertEquals("public/", configuration.getAssetsPrefix());
    assertEquals("images/", configuration.getIconsPrefix());
    assertEquals(List.of("home.html", "index.htm"), configuration.getIndexes());
    assertEquals(List.of("", ".html"), configuration.getExtensions());
    assertEquals("max-age=60", configuration.getCacheControl());
    assertEquals(Map.of("custom", "text/custom"), configuration.getMimeExtensions());
    assertEquals(Optional.of(true), configuration.getQuietMode());
    assertEquals(Optional.of(false), configuration.getShowBrowserWarning());
    assertEquals(Optional.of(true), configuration.getManageBrowserHistory());
    assertTrue(configuration.isClientConfirmationDialogShown());
    assertFalse(configuration.isConsoleDisallowed());
    assertEquals(Optional.of("config/app.cfg"), configuration.getConfigFile());
    assertEquals(List.of("--first", "second"), configuration.getProgramArguments());
  }

  @ParameterizedTest
  @ValueSource(strings = {"/", "//", " "})
  void shouldReadNoAssetsPrefixWhenAssetsDirIsTheRoot(String assetsDir) throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Files.writeString(app.resolve(CONF_PATH), """
        webforj.entry = "org.example.Application"
        webforj.assetsDir = "%s"
        """.formatted(assetsDir));
    Path jar = HelloApp.writeJar(app);

    ApplicationConfiguration configuration = ApplicationConfiguration.load(jar);

    assertEquals("", configuration.getAssetsPrefix());
  }

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldReadTheSecureSetting(boolean secure) throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Files.writeString(app.resolve(CONF_PATH), """
        webforj.entry = "org.example.Application"
        webforj.secure = %s
        """.formatted(secure));
    Path jar = HelloApp.writeJar(app);

    ApplicationConfiguration configuration = ApplicationConfiguration.load(jar);

    assertEquals(Optional.of(secure), configuration.getSecure());
  }

  @Test
  void shouldReadSingleStringProgramArgumentsAsOneArgument() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Files.writeString(app.resolve(CONF_PATH), """
        webforj.entry = "org.example.Application"
        webforj.programArguments = "--only"
        """);
    Path jar = HelloApp.writeJar(app);

    ApplicationConfiguration configuration = ApplicationConfiguration.load(jar);

    assertEquals(List.of("--only"), configuration.getProgramArguments());
  }

  @Test
  void shouldHandOutUnmodifiableIndexesAndProgramArguments() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Files.writeString(app.resolve(CONF_PATH), """
        webforj.entry = "org.example.Application"
        webforj.assetsIndex = ["home.html"]
        webforj.programArguments = ["--one"]
        """);
    Path jar = HelloApp.writeJar(app);

    ApplicationConfiguration configuration = ApplicationConfiguration.load(jar);

    List<String> indexes = configuration.getIndexes();
    List<String> arguments = configuration.getProgramArguments();
    assertThrows(UnsupportedOperationException.class, indexes::clear);
    assertThrows(UnsupportedOperationException.class, arguments::clear);
  }

  @Test
  void shouldFailWhenTheJarCannotBeRead() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Path jar = app.resolve("src/main/resources/static/app.css");

    assertThrows(IOException.class, () -> ApplicationConfiguration.load(jar));
  }

  @Test
  void shouldReadNoConfigurationFromMissingJar() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Path jar = app.resolve("target/hello-app-1.0-SNAPSHOT.jar");

    ApplicationConfiguration configuration = ApplicationConfiguration.load(jar);

    assertEquals(Optional.empty(), configuration.getEntryClass());
  }

  @Test
  void shouldFailWithMessageWhenWebforjConfIsInvalid() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Files.writeString(app.resolve(CONF_PATH), """
        webforj.entry = "org.example.Application"
        webforj.quiet = maybe
        """);
    Path jar = HelloApp.writeJar(app);

    IOException failure = assertThrows(IOException.class, () -> ApplicationConfiguration.load(jar));

    assertTrue(failure.getMessage().startsWith("BBj deployment webforj.conf is invalid. "));
  }

  @Test
  void shouldReadWebforjConfOfClassFolder() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Path classFolder = app.resolve(RESOURCES_FOLDER);
    Files.writeString(classFolder.resolve(CONF_FILE), QUIET_ENTRY_CONF);

    ApplicationConfiguration configuration = ApplicationConfiguration.load(classFolder);

    assertEquals(Optional.of(ENTRY_CLASS), configuration.getEntryClass());
    assertEquals(Optional.of(true), configuration.getQuietMode());
  }

  @Test
  void shouldReadWebforjConfOfTheFirstClassFolderThatHasIt() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Path classes = app.resolve("src/main/java");
    Path resources = app.resolve(RESOURCES_FOLDER);
    Files.writeString(resources.resolve(CONF_FILE), QUIET_ENTRY_CONF);

    ApplicationConfiguration configuration = ApplicationConfiguration.load(classes, resources);

    assertEquals(Optional.of(ENTRY_CLASS), configuration.getEntryClass());
    assertEquals(Optional.of(true), configuration.getQuietMode());
  }

  @Test
  void shouldReadNoConfigurationFromMissingSource() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Path missing = temporary.resolve("removed-classes");
    Path resources = app.resolve(RESOURCES_FOLDER);
    Files.writeString(resources.resolve(CONF_FILE), QUIET_ENTRY_CONF);

    ApplicationConfiguration configuration = ApplicationConfiguration.load(missing, resources);

    assertEquals(Optional.of(ENTRY_CLASS), configuration.getEntryClass());
    assertEquals(Optional.of(true), configuration.getQuietMode());
  }

  @Test
  void shouldUseDefaultsWhenTheClassFolderHasNoWebforjConf() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Path classFolder = app.resolve(RESOURCES_FOLDER);
    Files.delete(classFolder.resolve(CONF_FILE));

    ApplicationConfiguration configuration = ApplicationConfiguration.load(classFolder);

    assertEquals(Optional.empty(), configuration.getEntryClass());
    assertEquals(Optional.empty(), configuration.getQuietMode());
    assertTrue(configuration.isConsoleDisallowed());
  }

  @Test
  void shouldFailWithMessageWhenWebforjConfOfClassFolderIsInvalid() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Path classFolder = app.resolve(RESOURCES_FOLDER);
    Files.writeString(classFolder.resolve(CONF_FILE), """
        webforj.entry = "org.example.Application"
        webforj.quiet = maybe
        """);

    IOException failure =
        assertThrows(IOException.class, () -> ApplicationConfiguration.load(classFolder));

    assertEquals("BBj deployment webforj.conf is invalid. Reader: 2: webforj.quiet has type "
        + "STRING rather than BOOLEAN", failure.getMessage());
  }
}
