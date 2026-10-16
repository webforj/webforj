package com.webforj.plugin.maven;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.App;
import com.webforj.bbjship.foundation.BBjShipClient;
import com.webforj.bbjship.foundation.HelloApp;
import com.webforj.bbjship.foundation.model.BBjCredentials;
import com.webforj.plugin.maven.bbj.model.BBjAdminOptions;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Set;
import java.util.function.Consumer;
import java.util.stream.Stream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.xpath.XPathFactory;
import org.apache.maven.artifact.DefaultArtifact;
import org.apache.maven.artifact.handler.DefaultArtifactHandler;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.descriptor.PluginDescriptor;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.Answers;
import org.mockito.ArgumentCaptor;
import org.w3c.dom.Document;

class BBjDeployMojoTest {

  private static final String APPLICATION_NAME = "hello-app";
  private static final String WEBFORJ_VERSION = "26.03-SNAPSHOT";
  private static final String DEPLOYMENT_URL = "https://bbj.example.test/webforj-bbjship";
  private static final String ADMIN_PASSWORD = "secret";
  private static final String ARCHIVE_PATH = "target/bbj/hello-app.zip";
  private static final String ARCHIVE_DIRECTORY = "target/bbj";
  private static final String DEPLOYER_USERNAME = "deployer";

  @TempDir
  Path temporary;

  Path app;
  Path jar;
  Path foundation;
  Log log;
  BBjDeployMojo mojo;
  BBjShipClient bbjShip;
  BBjShipClient.Builder bbjShipBuilder;

  @BeforeEach
  void createMojo() throws Exception {
    app = HelloApp.createCopy(temporary);
    jar = HelloApp.writeJar(app);
    foundation = Path.of(App.class.getProtectionDomain().getCodeSource().getLocation().toURI());
    DefaultArtifact application = new DefaultArtifact("org.example", APPLICATION_NAME,
        "1.0-SNAPSHOT", null, "jar", null, new DefaultArtifactHandler("jar"));
    application.setFile(jar.toFile());
    DefaultArtifact framework = new DefaultArtifact("com.webforj", "webforj-foundation",
        WEBFORJ_VERSION, "compile", "jar", null, new DefaultArtifactHandler("jar"));
    framework.setFile(foundation.toFile());
    mojo = new BBjDeployMojo();
    mojo.project = new MavenProject();
    mojo.project.getBuild().setDirectory(app.resolve("target").toString());
    mojo.project.setArtifact(application);
    mojo.project.setArtifacts(Set.of(framework));
    mojo.pluginDescriptor = new PluginDescriptor();
    mojo.pluginDescriptor.setVersion(WEBFORJ_VERSION);
    mojo.bbj.setPublishName(APPLICATION_NAME);
    mojo.bbj.getDeployment().setUrl(DEPLOYMENT_URL);
    mojo.bbj.getAdmin().setUsername("administrator");
    mojo.bbj.getAdmin().setPassword(ADMIN_PASSWORD);
    log = mock(Log.class);
    mojo.setLog(log);
    bbjShip = mock(BBjShipClient.class);
    bbjShipBuilder = mock(BBjShipClient.Builder.class, Answers.RETURNS_SELF);
    when(bbjShipBuilder.build()).thenReturn(bbjShip);
  }

  @Test
  void shouldDeployTheArchiveOfTheHelloAppWithTheConfiguredSettings() throws Exception {
    Path archive = app.resolve(ARCHIVE_PATH);
    when(bbjShip.deploy(archive, APPLICATION_NAME))
        .thenReturn(URI.create("https://bbj.example.test/webapp/hello-app"));

    try (var bbjShips = mockStatic(BBjShipClient.class)) {
      bbjShips.when(() -> BBjShipClient.create(URI.create(DEPLOYMENT_URL)))
          .thenReturn(bbjShipBuilder);

      mojo.execute();
    }

    ArgumentCaptor<BBjCredentials> credentials = ArgumentCaptor.forClass(BBjCredentials.class);
    verify(bbjShipBuilder).setCredentials(credentials.capture());
    assertEquals("administrator", credentials.getValue().getUsername());
    assertEquals(ADMIN_PASSWORD, credentials.getValue().getPassword());
    verify(bbjShipBuilder).setVersion(WEBFORJ_VERSION);
    ArgumentCaptor<Consumer<String>> logChannel = ArgumentCaptor.captor();
    verify(bbjShipBuilder).setLog(logChannel.capture());
    logChannel.getValue().accept("Uploaded 50 percent of hello-app");
    verify(log).info("Uploaded 50 percent of hello-app");
    ArgumentCaptor<Consumer<String>> warnChannel = ArgumentCaptor.captor();
    verify(bbjShipBuilder).setWarn(warnChannel.capture());
    warnChannel.getValue().accept("Credentials are readable over plain HTTP");
    verify(log).warn("Credentials are readable over plain HTTP");
    verify(bbjShip).deploy(archive, APPLICATION_NAME);
    try (ZipFile zip = new ZipFile(archive.toFile())) {
      assertEquals(List.of("dependencies/00000-" + foundation.getFileName(), "application.jar",
          "bbj/webforj.bbj"), zip.stream().map(ZipEntry::getName).toList());
      try (InputStream packed = zip.getInputStream(zip.getEntry("application.jar"))) {
        assertArrayEquals(Files.readAllBytes(jar), packed.readAllBytes());
      }
    }

    verify(log).info("Deploying hello-app to https://bbj.example.test/webforj-bbjship");
    verify(log)
        .info("BBj deployment of hello-app completed at https://bbj.example.test/webapp/hello-app");
  }

  @ParameterizedTest
  @MethodSource("getDependencyResolutions")
  void shouldDeclareTheDependencyResolutionOfEachBbjGoal(String goal, String resolution)
      throws Exception {
    Document descriptor;
    try (InputStream input =
        BBjDeployMojoTest.class.getResourceAsStream("/META-INF/maven/plugin.xml")) {
      descriptor = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(input);
    }

    String declared = XPathFactory.newInstance().newXPath().evaluate(
        "/plugin/mojos/mojo[goal='%s']/requiresDependencyResolution".formatted(goal), descriptor);

    assertEquals(resolution, declared);
  }

  @ParameterizedTest
  @MethodSource("getDefaultPhases")
  void shouldDeclareTheDefaultPhaseOfEachBbjGoal(String goal, String phase) throws Exception {
    Document descriptor;
    try (InputStream input =
        BBjDeployMojoTest.class.getResourceAsStream("/META-INF/maven/plugin.xml")) {
      descriptor = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(input);
    }

    String declared = XPathFactory.newInstance().newXPath()
        .evaluate("/plugin/mojos/mojo[goal='%s']/phase".formatted(goal), descriptor);

    assertEquals(phase, declared);
  }

  @Test
  void shouldPublishUnderTheBuildFinalNameWhenNoPublishNameIsConfigured() throws Exception {
    mojo.bbj.setPublishName("");
    mojo.project.getBuild().setFinalName("hello-app-1.0-SNAPSHOT");

    try (var bbjShips = mockStatic(BBjShipClient.class)) {
      bbjShips.when(() -> BBjShipClient.create(URI.create(DEPLOYMENT_URL)))
          .thenReturn(bbjShipBuilder);

      mojo.execute();
    }

    verify(bbjShip).deploy(app.resolve("target/bbj/hello-app-1.0-SNAPSHOT.zip"),
        "hello-app-1.0-SNAPSHOT");
  }

  @ParameterizedTest
  @MethodSource("getIncompleteSettings")
  void shouldRefuseToDeployWithoutUrlOrPublishName(String url, String publishName, String message) {
    mojo.bbj.getDeployment().setUrl(url);
    mojo.bbj.setPublishName(publishName);

    try (var bbjShips = mockStatic(BBjShipClient.class)) {
      MojoExecutionException failure = assertThrows(MojoExecutionException.class, mojo::execute);

      assertEquals(message, failure.getMessage());
      bbjShips.verifyNoInteractions();
    }

    assertFalse(Files.exists(app.resolve(ARCHIVE_DIRECTORY)));
  }

  @ParameterizedTest
  @MethodSource("getRefusedPublishNames")
  void shouldRefuseInvalidPublishNameBeforeWritingTheArchive(String publishName, String message) {
    mojo.bbj.setPublishName(publishName);

    try (var bbjShips = mockStatic(BBjShipClient.class)) {
      MojoExecutionException failure = assertThrows(MojoExecutionException.class, mojo::execute);

      assertEquals(message, failure.getMessage());
      bbjShips.verifyNoInteractions();
    }

    assertFalse(Files.exists(app.resolve(ARCHIVE_DIRECTORY)));
    assertFalse(Files.exists(app.resolve("target/escaped.zip")));
  }

  @Test
  void shouldReportTheFailureOfTheBbjShip() throws Exception {
    IOException uploadFailure = new IOException("BBj deployment upload failed");
    when(bbjShip.deploy(app.resolve(ARCHIVE_PATH), APPLICATION_NAME)).thenThrow(uploadFailure);

    try (var bbjShips = mockStatic(BBjShipClient.class)) {
      bbjShips.when(() -> BBjShipClient.create(URI.create(DEPLOYMENT_URL)))
          .thenReturn(bbjShipBuilder);

      MojoExecutionException failure = assertThrows(MojoExecutionException.class, mojo::execute);

      assertEquals("BBj deployment failed. Check the application package, dependencies and "
          + "deployment settings", failure.getMessage());
      assertSame(uploadFailure, failure.getCause());
    }
  }

  @Test
  void shouldPassTheTokenInPlaceOfUsernameAndPassword() throws Exception {
    mojo.token = "4f2a9c";

    try (var bbjShips = mockStatic(BBjShipClient.class)) {
      bbjShips.when(() -> BBjShipClient.create(URI.create(DEPLOYMENT_URL)))
          .thenReturn(bbjShipBuilder);

      mojo.execute();
    }

    ArgumentCaptor<BBjCredentials> credentials = ArgumentCaptor.forClass(BBjCredentials.class);
    verify(bbjShipBuilder).setCredentials(credentials.capture());
    assertTrue(credentials.getValue().isToken());
    assertEquals("4f2a9c", credentials.getValue().getToken());
    verify(bbjShip).deploy(app.resolve(ARCHIVE_PATH), APPLICATION_NAME);
  }

  @Test
  void shouldPreferTheCommandLineOverTheConfiguration() throws Exception {
    mojo.deploymentUrl = "http://localhost:8888/webforj-bbjship";
    mojo.publishName = "hello-app-staging";
    mojo.username = DEPLOYER_USERNAME;
    mojo.password = "deployer-secret";

    try (var bbjShips = mockStatic(BBjShipClient.class)) {
      bbjShips.when(() -> BBjShipClient.create(URI.create("http://localhost:8888/webforj-bbjship")))
          .thenReturn(bbjShipBuilder);

      mojo.execute();
    }

    ArgumentCaptor<BBjCredentials> credentials = ArgumentCaptor.forClass(BBjCredentials.class);
    verify(bbjShipBuilder).setCredentials(credentials.capture());
    assertEquals(DEPLOYER_USERNAME, credentials.getValue().getUsername());
    assertEquals("deployer-secret", credentials.getValue().getPassword());
    verify(bbjShip).deploy(app.resolve("target/bbj/hello-app-staging.zip"), "hello-app-staging");
  }

  @Test
  void shouldRefuseProjectThatIsNotPackagedAsJar() {
    mojo.project.setPackaging("war");

    try (var bbjShips = mockStatic(BBjShipClient.class)) {
      MojoExecutionException failure = assertThrows(MojoExecutionException.class, mojo::execute);

      assertEquals("BBj deployment requires jar packaging, this project is packaged as war",
          failure.getMessage());
      bbjShips.verifyNoInteractions();
    }

    assertFalse(Files.exists(app.resolve(ARCHIVE_DIRECTORY)));
  }

  @Test
  void shouldRefuseToDeployWhenTheArtifactHasNoFile() {
    mojo.project.getArtifact().setFile(null);

    try (var bbjShips = mockStatic(BBjShipClient.class)) {
      MojoExecutionException failure = assertThrows(MojoExecutionException.class, mojo::execute);

      assertEquals(
          "BBj deployment requires the packaged application jar. Run the package phase "
              + "in the same invocation, as in mvn package webforj:bbj-deploy",
          failure.getMessage());
      bbjShips.verifyNoInteractions();
    }

    assertFalse(Files.exists(app.resolve(ARCHIVE_DIRECTORY)));
  }

  @Test
  void shouldRejectAnInvalidUrlBeforeWritingTheArchive() {
    mojo.bbj.getDeployment().setUrl("https://bbj.example.test/%invalid");

    try (var bbjShips = mockStatic(BBjShipClient.class)) {
      MojoExecutionException failure = assertThrows(MojoExecutionException.class, mojo::execute);

      assertEquals("BBj deployment failed. Check the application package, dependencies and "
          + "deployment settings", failure.getMessage());
      bbjShips.verifyNoInteractions();
    }

    assertFalse(Files.exists(app.resolve(ARCHIVE_PATH)));
  }

  @ParameterizedTest
  @MethodSource("getAccounts")
  void shouldWarnWhileTheFactoryAccountIsInUse(String username, String password, int warnings)
      throws Exception {
    mojo.bbj.setAdmin(new BBjAdminOptions());
    mojo.username = username;
    mojo.password = password;

    try (var bbjShips = mockStatic(BBjShipClient.class)) {
      bbjShips.when(() -> BBjShipClient.create(URI.create(DEPLOYMENT_URL)))
          .thenReturn(bbjShipBuilder);

      mojo.execute();
    }

    verify(log, times(warnings))
        .warn("Warning: the BBj factory administrator account admin is in use with its "
            + "factory password. Change the password in Enterprise Manager and configure the new "
            + "credentials");
  }

  @ParameterizedTest
  @MethodSource("getIncompleteCredentials")
  void shouldRefuseIncompleteCredentials(String username, String password, String missing) {
    mojo.bbj.setAdmin(new BBjAdminOptions());
    mojo.username = username;
    mojo.password = password;

    MojoExecutionException failure = assertThrows(MojoExecutionException.class, mojo::execute);

    assertEquals(missing + ". Configure bbj.admin.username and bbj.admin.password, or "
        + "webforj.bbj.admin.token", failure.getMessage());
  }

  private static Stream<Arguments> getAccounts() {
    return Stream.of(Arguments.of(null, null, 1), Arguments.of("admin", "admin123", 1),
        Arguments.of("admin", ADMIN_PASSWORD, 0));
  }

  private static Stream<Arguments> getIncompleteCredentials() {
    return Stream.of(
        Arguments.of(null, ADMIN_PASSWORD, "The BBj administration user name is missing"),
        Arguments.of(DEPLOYER_USERNAME, null, "The BBj administration password is missing"));
  }

  private static Stream<Arguments> getDependencyResolutions() {
    return Stream.of(Arguments.of("bbj-deploy", "runtime"), Arguments.of("bbj-package", "runtime"),
        Arguments.of("bbj-undeploy", ""), Arguments.of("bbj-setup", ""));
  }

  private static Stream<Arguments> getDefaultPhases() {
    return Stream.of(Arguments.of("bbj-deploy", "install"), Arguments.of("bbj-package", "package"),
        Arguments.of("bbj-undeploy", ""), Arguments.of("bbj-setup", ""));
  }

  private static Stream<Arguments> getIncompleteSettings() {
    return Stream.of(
        Arguments.of("", APPLICATION_NAME,
            "BBj deployment requires bbj.deployment.url. Set it in plugin configuration or "
                + "pass -Dwebforj.bbj.deployment.url=<URL>"),
        Arguments.of(DEPLOYMENT_URL, "",
            "BBj deployment requires bbj.publishName or a final name of the build"));
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
