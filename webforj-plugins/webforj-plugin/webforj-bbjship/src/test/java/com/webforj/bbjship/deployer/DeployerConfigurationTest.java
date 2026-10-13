package com.webforj.bbjship.deployer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.api.admin.BBjAdminAppDeploymentApplication;
import com.basis.api.admin.BBjAdminAppDeploymentConfiguration;
import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.BBjAdminException;
import com.basis.api.admin.BBjAdminList;
import com.basis.api.admin.rest.adapter.BBjAdminAdapterList;
import com.typesafe.config.ConfigException;
import com.webforj.bbjship.ClasspathRelease;
import com.webforj.bbjship.Deployer;
import com.webforj.bbjship.DeploymentException;
import com.webforj.bbjship.DevelopmentRunFiles;
import com.webforj.plugin.foundation.bbj.DeploymentArchive;
import com.webforj.plugin.foundation.bbj.HelloApp;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.InputStream;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;

final class DeployerConfigurationTest {

  private static final String NAME = "hello-app";
  private static final String WEBFORJ_CONF = "src/main/resources/webforj.conf";
  private static final String ARCHIVE = "hello-app.zip";
  private static final String DEPLOYMENTS = "deployments";
  private static final String DEVELOPMENT_PROPERTIES = "development.properties";
  private static final String CURRENT_FILE = "hello-app/current";
  private static final String CLASS_FOLDER_PROPERTY = """
      classFolder=%s
      """;

  @TempDir
  Path temporary;

  private BBjAdminBase admin;
  private BBjAdminAppDeploymentApplication application;

  @BeforeEach
  void initServerWithoutApplications() throws BBjAdminException {
    admin = mock(BBjAdminBase.class);
    BBjAdminAppDeploymentConfiguration configuration =
        mock(BBjAdminAppDeploymentConfiguration.class);
    application = mock(BBjAdminAppDeploymentApplication.class);
    when(admin.getRemoteConfiguration()).thenReturn(configuration);
    when(admin.getClasspathNames()).thenReturn(new BBjAdminAdapterList<>(List.of()));
    when(admin.getBBjProcesses()).thenReturn(List.of());
    when(configuration.getApplications()).thenReturn(new BBjAdminAdapterList<>(List.of()));
    when(configuration.createApplication()).thenReturn(application);
  }

  @Test
  void shouldRegisterEverySettingOfWebforjConf() throws Exception {
    Path app = HelloApp.createCopy(temporary.resolve(NAME));
    Files.writeString(app.resolve(WEBFORJ_CONF), """
        webforj {
          entry = "org.example.Application"
          quiet = true
          showBrowserWarning = false
          manageBrowserHistory = true
          showClientConfirmationDialog = true
          disallowConsole = false
        }
        """);
    Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE));

    Deployer.deploy(new ByteArrayInputStream(Files.readAllBytes(archive)), NAME,
        temporary.resolve(DEPLOYMENTS).toString(), admin, ClasspathRelease.NONE);

    verify(application).setBoolean(BBjAdminAppDeploymentApplication.QUIET, true);
    verify(application).setBoolean(BBjAdminAppDeploymentApplication.SHOW_BROWSER_WARNING, false);
    verify(application)
        .setBoolean(BBjAdminAppDeploymentApplication.SHOW_BROWSER_WARNING_USE_DEFAULT, false);
    verify(application).setBoolean(BBjAdminAppDeploymentApplication.MANAGE_BROWSER_HISTORY, true);
    verify(application)
        .setBoolean(BBjAdminAppDeploymentApplication.MANAGE_BROWSER_HISTORY_USE_DEFAULT, false);
    verify(application).setBoolean(BBjAdminAppDeploymentApplication.SHOW_CONFIRM_CLOSE_DIALOG,
        true);
    verify(application)
        .setBoolean(BBjAdminAppDeploymentApplication.SHOW_CONFIRM_CLOSE_DIALOG_USE_DEFAULT, false);
    verify(application).setBoolean(BBjAdminAppDeploymentApplication.DISALLOW_CONSOLE, false);
    verify(application).setBoolean(BBjAdminAppDeploymentApplication.DISALLOW_CONSOLE_USE_DEFAULT,
        false);
    verify(application, never()).setBoolean(eq(BBjAdminAppDeploymentApplication.SECURE),
        anyBoolean());
    verify(application, never()).setBoolean(eq(BBjAdminAppDeploymentApplication.SECURE_USE_DEFAULT),
        anyBoolean());
  }

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldRegisterTheSecureSettingInPlaceOfTheServerDefault(boolean secure) throws Exception {
    Path app = HelloApp.createCopy(temporary.resolve(NAME));
    Files.writeString(app.resolve(WEBFORJ_CONF), """
        webforj.entry = "org.example.Application"
        webforj.secure = %s
        """.formatted(secure));
    Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE));

    Deployer.deploy(new ByteArrayInputStream(Files.readAllBytes(archive)), NAME,
        temporary.resolve(DEPLOYMENTS).toString(), admin, ClasspathRelease.NONE);

    verify(application).setBoolean(BBjAdminAppDeploymentApplication.SECURE, secure);
    verify(application).setBoolean(BBjAdminAppDeploymentApplication.SECURE_USE_DEFAULT, false);
  }

  @Test
  void shouldRegisterTheDefaultsWhenTheApplicationHasNoWebforjConf() throws Exception {
    Path app = HelloApp.createCopy(temporary.resolve(NAME));
    Files.delete(app.resolve(WEBFORJ_CONF));
    Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE));

    Deployer.deploy(new ByteArrayInputStream(Files.readAllBytes(archive)), NAME,
        temporary.resolve(DEPLOYMENTS).toString(), admin, ClasspathRelease.NONE);

    verify(application).setBoolean(BBjAdminAppDeploymentApplication.SHOW_CONFIRM_CLOSE_DIALOG,
        false);
    verify(application).setBoolean(BBjAdminAppDeploymentApplication.DISALLOW_CONSOLE, true);
    verify(application, never()).setBoolean(eq(BBjAdminAppDeploymentApplication.QUIET),
        anyBoolean());
    verify(application, never())
        .setBoolean(eq(BBjAdminAppDeploymentApplication.SHOW_BROWSER_WARNING), anyBoolean());
    verify(application, never())
        .setBoolean(eq(BBjAdminAppDeploymentApplication.MANAGE_BROWSER_HISTORY), anyBoolean());
    verify(application, never()).setString(eq(BBjAdminAppDeploymentApplication.CONFIG_FILE),
        anyString());
    verify(application, never()).getArguments();
  }

  @Test
  void shouldRegisterTheAbsoluteConfigFileAsItIs() throws Exception {
    Path configFile = temporary.toAbsolutePath().resolve("elsewhere/app.cfg");
    Path app = HelloApp.createCopy(temporary.resolve(NAME));
    Files.writeString(app.resolve(WEBFORJ_CONF), """
        webforj.entry = "org.example.Application"
        webforj.configFile = "%s"
        """.formatted(configFile.toString().replace("\\", "\\\\")));
    Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE));
    Path root = temporary.resolve(DEPLOYMENTS);

    Deployer.deploy(new ByteArrayInputStream(Files.readAllBytes(archive)), NAME, root.toString(),
        admin, ClasspathRelease.NONE);

    verify(application).setString(BBjAdminAppDeploymentApplication.CONFIG_FILE,
        configFile.toString());
  }

  @ParameterizedTest
  @ValueSource(strings = {"config/app.cfg", "../outside.cfg", "cfg/config.bbx"})
  void shouldStoreTheConfigFileAsWritten(String configFile) throws Exception {
    Path app = HelloApp.createCopy(temporary.resolve(NAME));
    Files.writeString(app.resolve(WEBFORJ_CONF), """
        webforj.entry = "org.example.Application"
        webforj.configFile = "%s"
        """.formatted(configFile));
    Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE));
    Path root = temporary.resolve(DEPLOYMENTS);

    Deployer.deploy(new ByteArrayInputStream(Files.readAllBytes(archive)), NAME, root.toString(),
        admin, ClasspathRelease.NONE);

    verify(application).setString(BBjAdminAppDeploymentApplication.CONFIG_FILE, configFile);
  }

  @ParameterizedTest
  @MethodSource("getInvalidWebforjConfs")
  void shouldRefuseInvalidWebforjConfBeforeAnyChange(String webforjConf, String message)
      throws Exception {
    Path app = HelloApp.createCopy(temporary.resolve(NAME));
    Files.writeString(app.resolve(WEBFORJ_CONF), webforjConf);
    Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE));
    Path root = temporary.resolve(DEPLOYMENTS);
    InputStream upload = new ByteArrayInputStream(Files.readAllBytes(archive));
    String destination = root.toString();

    DeploymentException failure = assertThrows(DeploymentException.class,
        () -> Deployer.deploy(upload, NAME, destination, admin, ClasspathRelease.NONE));

    assertEquals(message, failure.getMessage());
    assertInstanceOf(ConfigException.class, failure.getCause());
    verify(admin, never()).setClasspath(anyString(), any());
    verify(admin, never()).commit();
    verify(application, never()).commit();
    try (Stream<Path> directories = Files.list(root)) {
      assertEquals(List.of(root.resolve(".lock")), directories.toList());
    }
  }

  @Test
  void shouldRegisterTheProgramArgumentsInOrderInPlaceOfThePreviousOnes() throws Exception {
    BBjAdminList<String> arguments = new BBjAdminAdapterList<>(List.of("stale"));
    when(application.getArguments()).thenReturn(arguments);
    Path app = HelloApp.createCopy(temporary.resolve(NAME));
    Files.writeString(app.resolve(WEBFORJ_CONF), """
        webforj.entry = "org.example.Application"
        webforj.programArguments = ["--b", "--a"]
        """);
    Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE));

    Deployer.deploy(new ByteArrayInputStream(Files.readAllBytes(archive)), NAME,
        temporary.resolve(DEPLOYMENTS).toString(), admin, ClasspathRelease.NONE);

    assertEquals(List.of("--b", "--a"), arguments);
  }

  private static Stream<Arguments> getInvalidWebforjConfs() {
    return Stream.of(Arguments.of("""
        webforj.entry = "org.example.Application"
        webforj.quiet = [true
        """,
        "BBj deployment webforj.conf is invalid. Reader: 3: List should have had new element "
            + "after a comma, instead had token: end of file (if you want the comma or end of file "
            + "to be part of a string value, then double-quote it)"),
        Arguments.of("""
            webforj.entry = "org.example.Application"
            webforj.quiet = maybe
            """, "BBj deployment webforj.conf is invalid. Reader: 2: webforj.quiet has type STRING "
            + "rather than BOOLEAN"));
  }

  @Nested
  class DevelopmentRun {

    private Path classFolder;
    private Path development;

    @BeforeEach
    void initDevelopmentArchiveOfHelloAppWithDevelopmentRunsEnabled() throws Exception {
      Path app = HelloApp.createCopy(temporary.resolve(NAME));
      classFolder = app.resolve("src/main/resources");
      development = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE));
      try (FileSystem zip = FileSystems.newFileSystem(development)) {
        Files.delete(zip.getPath("application.jar"));
        Files.writeString(zip.getPath(DEVELOPMENT_PROPERTIES),
            CLASS_FOLDER_PROPERTY.formatted(classFolder.toString().replace("\\", "\\\\")));
      }

      Files.createFile(temporary.resolve("development.enabled"));
    }

    @Test
    void shouldRegisterTheWebforjConfOfTheClassFolder() throws Exception {
      Files.writeString(classFolder.resolve("webforj.conf"), """
          webforj.entry = "org.example.Application"
          webforj.secure = true
          """);

      Deployer.deploy(new ByteArrayInputStream(Files.readAllBytes(development)), NAME,
          temporary.resolve(DEPLOYMENTS).toString(), admin, ClasspathRelease.NONE);

      verify(application).setBoolean(BBjAdminAppDeploymentApplication.SECURE, true);
    }

    @Test
    void shouldRegisterTheClassFolderAfterTheDependenciesWithoutApplicationJar() throws Exception {
      Path root = temporary.resolve(DEPLOYMENTS);

      Deployer.deploy(new ByteArrayInputStream(Files.readAllBytes(development)), NAME,
          root.toString(), admin, ClasspathRelease.NONE);

      Path deployment = root.resolve(NAME).resolve(Files.readString(root.resolve(CURRENT_FILE)));
      List<String> classpath = new ArrayList<>(List.of("(_webforj_bbjship)"));
      try (Stream<Path> jars = Files.list(deployment.resolve("dependencies"))) {
        jars.sorted().map(Path::toString).forEach(classpath::add);
      }

      classpath.add(classFolder.toString());
      verify(admin).setClasspath("webforj_bbjship_hello-app", classpath);
    }

    @Test
    void shouldWriteTheClassFolderIntoTheDevelopmentMarkerOfTheDeployment() throws Exception {
      Path root = temporary.resolve(DEPLOYMENTS);

      Deployer.deploy(new ByteArrayInputStream(Files.readAllBytes(development)), NAME,
          root.toString(), admin, ClasspathRelease.NONE);

      Path deployment = root.resolve(NAME).resolve(Files.readString(root.resolve(CURRENT_FILE)));
      assertEquals(Optional.of(List.of(classFolder.toString())),
          DeploymentArchive.findClassFolders(deployment));
    }

    @Test
    void shouldRegisterEveryClassFolderInOrderAfterTheDependencies() throws Exception {
      Path classes = classFolder.resolveSibling("java");
      try (FileSystem zip = FileSystems.newFileSystem(development)) {
        Files.writeString(zip.getPath(DEVELOPMENT_PROPERTIES), CLASS_FOLDER_PROPERTY
            .formatted((classes + File.pathSeparator + classFolder).replace("\\", "\\\\")));
      }

      Path root = temporary.resolve(DEPLOYMENTS);

      Deployer.deploy(new ByteArrayInputStream(Files.readAllBytes(development)), NAME,
          root.toString(), admin, ClasspathRelease.NONE);

      Path deployment = root.resolve(NAME).resolve(Files.readString(root.resolve(CURRENT_FILE)));
      List<String> classpath = new ArrayList<>(List.of("(_webforj_bbjship)"));
      try (Stream<Path> jars = Files.list(deployment.resolve("dependencies"))) {
        jars.sorted().map(Path::toString).forEach(classpath::add);
      }

      classpath.add(classes.toString());
      classpath.add(classFolder.toString());
      verify(admin).setClasspath("webforj_bbjship_hello-app", classpath);
    }

    @Test
    void shouldRegisterTheWebforjConfOfTheFirstClassFolderThatHasIt() throws Exception {
      Path classes = classFolder.resolveSibling("java");
      try (FileSystem zip = FileSystems.newFileSystem(development)) {
        Files.writeString(zip.getPath(DEVELOPMENT_PROPERTIES), CLASS_FOLDER_PROPERTY
            .formatted((classes + File.pathSeparator + classFolder).replace("\\", "\\\\")));
      }

      Files.writeString(classFolder.resolve("webforj.conf"), """
          webforj.entry = "org.example.Application"
          webforj.secure = true
          """);

      Deployer.deploy(new ByteArrayInputStream(Files.readAllBytes(development)), NAME,
          temporary.resolve(DEPLOYMENTS).toString(), admin, ClasspathRelease.NONE);

      verify(application).setBoolean(BBjAdminAppDeploymentApplication.SECURE, true);
    }

    @Test
    void shouldWriteEveryClassFolderIntoTheDevelopmentMarkerOneFolderPerLine() throws Exception {
      Path classes = classFolder.resolveSibling("java");
      try (FileSystem zip = FileSystems.newFileSystem(development)) {
        Files.writeString(zip.getPath(DEVELOPMENT_PROPERTIES), CLASS_FOLDER_PROPERTY
            .formatted((classes + File.pathSeparator + classFolder).replace("\\", "\\\\")));
      }

      Path root = temporary.resolve(DEPLOYMENTS);

      Deployer.deploy(new ByteArrayInputStream(Files.readAllBytes(development)), NAME,
          root.toString(), admin, ClasspathRelease.NONE);

      Path deployment = root.resolve(NAME).resolve(Files.readString(root.resolve(CURRENT_FILE)));
      assertEquals(Optional.of(List.of(classes.toString(), classFolder.toString())),
          DeploymentArchive.findClassFolders(deployment));
    }
  }
}
