package com.webforj.plugin.foundation.bbj;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.App;
import com.webforj.bbjship.foundation.ApplicationClasspath;
import com.webforj.bbjship.foundation.ApplicationClasspath.ResolvedJar;
import com.webforj.bbjship.foundation.BBjShipClient;
import com.webforj.bbjship.foundation.HelloApp;
import com.webforj.bbjship.foundation.Setup;
import com.webforj.bbjship.foundation.model.BBjCredentials;
import com.webforj.bbjship.foundation.model.JavaSettings;
import com.webforj.plugin.foundation.devtools.DevtoolsDelivery;
import com.webforj.plugin.foundation.resolve.ArtifactResolver;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.jar.JarOutputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.Answers;
import org.mockito.InOrder;
import org.mockito.MockedStatic;

class DevelopmentTest {

  private static final String APP_NAME = "hello-app";
  private static final String SNAPSHOT_VERSION = "26.03-SNAPSHOT";
  private static final String GROUP_ID = "com.webforj";
  private static final String DEPLOYMENT_URL = "https://bbj.example.test/webforj-bbjship";
  private static final String DEVELOPMENT_PROPERTIES = "development.properties";
  private static final String CLASS_FOLDER = "target/classes";

  @TempDir
  Path temporary;

  Path app;
  Path foundation;
  Path devtools;
  Path archive;
  Path bundle;
  List<String> lines;
  List<String> warnings;
  BBjCredentials credentials;
  ApplicationClasspath classpath;
  ArtifactResolver resolver;
  BBjShipClient bbjShip;
  BBjShipClient.Builder bbjShipBuilder;
  DevtoolsDelivery.Builder deliveryBuilder;
  Setup setup;
  Setup.Builder setupBuilder;

  @BeforeEach
  void createDevelopment() throws Exception {
    app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    foundation = Path.of(App.class.getProtectionDomain().getCodeSource().getLocation().toURI());
    devtools = temporary.resolve("webforj-devtools.jar");
    new JarOutputStream(Files.newOutputStream(devtools)).close();
    archive = app.resolve("target/bbj/hello-app.zip");
    bundle = temporary.resolve("webforj-bbjship-plugin-26.03-SNAPSHOT-bbj-plugin.zip");
    lines = new ArrayList<>();
    warnings = new ArrayList<>();
    credentials = BBjCredentials.createUser("administrator", "secret");
    classpath = new ApplicationClasspath(
        List.of(new ResolvedJar(GROUP_ID, "webforj-foundation", SNAPSHOT_VERSION, foundation)));
    resolver = mock(ArtifactResolver.class);
    bbjShip = mock(BBjShipClient.class);
    bbjShipBuilder = mock(BBjShipClient.Builder.class, Answers.RETURNS_SELF);
    when(bbjShipBuilder.build()).thenReturn(bbjShip);
    when(bbjShip.deploy(archive, APP_NAME))
        .thenReturn(URI.create("https://bbj.example.test/webapp/hello-app"));
    deliveryBuilder = mock(DevtoolsDelivery.Builder.class, Answers.RETURNS_SELF);
    DevtoolsDelivery delivery = mock(DevtoolsDelivery.class);
    when(deliveryBuilder.build()).thenReturn(delivery);
    when(delivery.getJars()).thenReturn(
        List.of(new ResolvedJar(GROUP_ID, "webforj-devtools", SNAPSHOT_VERSION, devtools)));
    setup = mock(Setup.class);
    setupBuilder = mock(Setup.Builder.class, Answers.RETURNS_SELF);
    when(setupBuilder.build()).thenReturn(setup);
  }

  @Test
  void shouldStartTheDevelopmentRunOfTheClassFolderWithTheDevtools() throws Exception {
    Development development = createBuilder().build();

    URI published;
    try (MockedStatic<BBjShipClient> bbjShips = mockStatic(BBjShipClient.class);
        MockedStatic<DevtoolsDelivery> deliveries = mockStatic(DevtoolsDelivery.class);
        MockedStatic<Setup> setups = mockStatic(Setup.class)) {
      bbjShips.when(() -> BBjShipClient.create(URI.create(DEPLOYMENT_URL)))
          .thenReturn(bbjShipBuilder);
      deliveries.when(DevtoolsDelivery::create).thenReturn(deliveryBuilder);
      setups.when(() -> Setup.create(bundle, SNAPSHOT_VERSION)).thenReturn(setupBuilder);

      published = development.start();
    }

    assertEquals(URI.create("https://bbj.example.test/webapp/hello-app"), published);
    verify(deliveryBuilder).setArtifactId("webforj-devtools");
    verify(deliveryBuilder).setApplicationClasspath(classpath);
    verify(deliveryBuilder).setResolver(resolver);
    verify(bbjShipBuilder).setCredentials(credentials);
    verify(bbjShipBuilder).setVersion(SNAPSHOT_VERSION);
    try (ZipFile zip = new ZipFile(archive.toFile())) {
      assertEquals(
          List.of("dependencies/00000-" + foundation.getFileName(),
              "dependencies/00001-webforj-devtools.jar", DEVELOPMENT_PROPERTIES, "bbj/webforj.bbj"),
          zip.stream().map(ZipEntry::getName).toList());
      try (InputStream input = zip.getInputStream(zip.getEntry(DEVELOPMENT_PROPERTIES))) {
        String text = new String(input.readAllBytes(), StandardCharsets.UTF_8);
        assertEquals("classFolder=" + app.resolve(CLASS_FOLDER) + System.lineSeparator(),
            text.substring(text.indexOf('\n') + 1));
      }
    }

    assertEquals(List.of(), lines);
  }

  @Test
  void shouldNameEveryClassFolderOfTheDevelopmentRunInOrder() throws Exception {
    Path classes = app.resolve("build/classes/java/main");
    Path resources = app.resolve("build/resources/main");
    Development development = createBuilder().setClassFolders(List.of(classes, resources)).build();

    try (MockedStatic<BBjShipClient> bbjShips = mockStatic(BBjShipClient.class);
        MockedStatic<DevtoolsDelivery> deliveries = mockStatic(DevtoolsDelivery.class);
        MockedStatic<Setup> setups = mockStatic(Setup.class)) {
      bbjShips.when(() -> BBjShipClient.create(URI.create(DEPLOYMENT_URL)))
          .thenReturn(bbjShipBuilder);
      deliveries.when(DevtoolsDelivery::create).thenReturn(deliveryBuilder);
      setups.when(() -> Setup.create(bundle, SNAPSHOT_VERSION)).thenReturn(setupBuilder);

      development.start();
    }

    Properties properties = new Properties();
    try (ZipFile zip = new ZipFile(archive.toFile());
        InputStream input = zip.getInputStream(zip.getEntry(DEVELOPMENT_PROPERTIES))) {
      properties.load(input);
    }

    assertEquals(classes + File.pathSeparator + resources, properties.getProperty("classFolder"));
  }

  @Test
  void shouldRefuseDevelopmentRunWithoutClassFolders() {
    Development development = createBuilder().setClassFolders(List.of()).build();

    IllegalArgumentException failure;
    try (MockedStatic<DevtoolsDelivery> deliveries = mockStatic(DevtoolsDelivery.class);
        MockedStatic<Setup> setups = mockStatic(Setup.class)) {
      deliveries.when(DevtoolsDelivery::create).thenReturn(deliveryBuilder);
      setups.when(() -> Setup.create(bundle, SNAPSHOT_VERSION)).thenReturn(setupBuilder);

      failure = assertThrows(IllegalArgumentException.class, development::start);
    }

    assertEquals("a development run needs at least one class folder", failure.getMessage());
  }

  @Test
  void shouldRefuseRelativeClassFolder() {
    Development.Builder builder = createBuilder();
    Path relative = Path.of("build", "resources");
    List<Path> classFolders = List.of(app.resolve(CLASS_FOLDER), relative);

    IllegalArgumentException failure =
        assertThrows(IllegalArgumentException.class, () -> builder.setClassFolders(classFolders));

    assertEquals("a class folder of a development run must be absolute: " + relative,
        failure.getMessage());
  }

  @Test
  void shouldCarryTheRuntimeClasspathAndFindTheDevtoolsAgainstTheApplicationClasspath()
      throws Exception {
    Path engine = temporary.resolve("webforj-engine.jar");
    new JarOutputStream(Files.newOutputStream(engine)).close();
    ApplicationClasspath application = new ApplicationClasspath(
        List.of(new ResolvedJar(GROUP_ID, "webforj-foundation", SNAPSHOT_VERSION, foundation),
            new ResolvedJar(GROUP_ID, "webforj-engine", SNAPSHOT_VERSION, engine)));
    Development development = createBuilder().setApplicationClasspath(application).build();

    try (MockedStatic<BBjShipClient> bbjShips = mockStatic(BBjShipClient.class);
        MockedStatic<DevtoolsDelivery> deliveries = mockStatic(DevtoolsDelivery.class);
        MockedStatic<Setup> setups = mockStatic(Setup.class)) {
      bbjShips.when(() -> BBjShipClient.create(URI.create(DEPLOYMENT_URL)))
          .thenReturn(bbjShipBuilder);
      deliveries.when(DevtoolsDelivery::create).thenReturn(deliveryBuilder);
      setups.when(() -> Setup.create(bundle, SNAPSHOT_VERSION)).thenReturn(setupBuilder);

      development.start();
    }

    verify(deliveryBuilder).setApplicationClasspath(application);
    try (ZipFile zip = new ZipFile(archive.toFile())) {
      assertEquals(
          List.of("dependencies/00000-" + foundation.getFileName(),
              "dependencies/00001-webforj-devtools.jar", DEVELOPMENT_PROPERTIES, "bbj/webforj.bbj"),
          zip.stream().map(ZipEntry::getName).toList());
    }
  }

  @Test
  void shouldSetUpTheBbjShipForDevelopmentRunsBeforeTheDeploy() throws Exception {
    Path home = temporary.resolve("bbx");
    JavaSettings javaSettings =
        new JavaSettings(home.resolve("cfg/BBj.properties"), temporary.resolve("hotswap"));
    Development development = createBuilder().setHome(home).setJavaSettings(javaSettings).build();

    try (
        MockedStatic<BBjShipClient> bbjShips =
            mockStatic(BBjShipClient.class, Answers.CALLS_REAL_METHODS);
        MockedStatic<DevtoolsDelivery> deliveries = mockStatic(DevtoolsDelivery.class);
        MockedStatic<Setup> setups = mockStatic(Setup.class)) {
      bbjShips.when(() -> BBjShipClient.create(URI.create(DEPLOYMENT_URL)))
          .thenReturn(bbjShipBuilder);
      deliveries.when(DevtoolsDelivery::create).thenReturn(deliveryBuilder);
      setups.when(() -> Setup.create(bundle, SNAPSHOT_VERSION)).thenReturn(setupBuilder);

      development.start();
    }

    InOrder order = inOrder(setup, bbjShip);
    order.verify(setup).apply();
    order.verify(bbjShip).deploy(archive, APP_NAME);
    verify(setupBuilder).setDeploymentUrl(URI.create(DEPLOYMENT_URL));
    verify(setupBuilder).setCredentials(credentials);
    verify(setupBuilder).setPort(443);
    verify(setupBuilder).setDevelopmentEnabled(true);
    verify(setupBuilder).setHome(home);
    verify(setupBuilder).setJavaSettings(javaSettings);
  }

  @Test
  void shouldLeaveTheHomeAndTheJavaSettingsToTheSetupWhenNoneAreGiven() throws Exception {
    Development development = createBuilder().build();

    try (MockedStatic<BBjShipClient> bbjShips = mockStatic(BBjShipClient.class);
        MockedStatic<DevtoolsDelivery> deliveries = mockStatic(DevtoolsDelivery.class);
        MockedStatic<Setup> setups = mockStatic(Setup.class)) {
      bbjShips.when(() -> BBjShipClient.create(URI.create(DEPLOYMENT_URL)))
          .thenReturn(bbjShipBuilder);
      deliveries.when(DevtoolsDelivery::create).thenReturn(deliveryBuilder);
      setups.when(() -> Setup.create(bundle, SNAPSHOT_VERSION)).thenReturn(setupBuilder);

      development.start();
    }

    verify(setupBuilder, never()).setHome(any());
    verify(setupBuilder, never()).setJavaSettings(any());
  }

  @Test
  void shouldDeployNothingWhenTheSetupFails() throws Exception {
    IOException refusal = new IOException("BBj setup is local only. Run setup on the BBj host");
    doThrow(refusal).when(setup).apply();
    Development development = createBuilder().build();

    try (MockedStatic<BBjShipClient> bbjShips = mockStatic(BBjShipClient.class);
        MockedStatic<Setup> setups = mockStatic(Setup.class)) {
      setups.when(() -> Setup.create(bundle, SNAPSHOT_VERSION)).thenReturn(setupBuilder);

      IOException failure = assertThrows(IOException.class, development::start);

      assertSame(refusal, failure);
      bbjShips.verify(() -> BBjShipClient.create(any()), never());
    }
  }

  @Test
  void shouldStopTheDevelopmentRun() throws Exception {
    Development development = createBuilder().build();
    try (MockedStatic<BBjShipClient> bbjShips = mockStatic(BBjShipClient.class);
        MockedStatic<DevtoolsDelivery> deliveries = mockStatic(DevtoolsDelivery.class);
        MockedStatic<Setup> setups = mockStatic(Setup.class)) {
      bbjShips.when(() -> BBjShipClient.create(URI.create(DEPLOYMENT_URL)))
          .thenReturn(bbjShipBuilder);
      deliveries.when(DevtoolsDelivery::create).thenReturn(deliveryBuilder);
      setups.when(() -> Setup.create(bundle, SNAPSHOT_VERSION)).thenReturn(setupBuilder);
      development.start();
    }

    development.stop();

    verify(bbjShip).undeploy(APP_NAME);
    assertEquals("webforJ watch: development run of hello-app stopped", lines.get(0));
  }

  @Test
  void shouldIgnoreStopBeforeStart() {
    Development development = createBuilder().build();

    assertDoesNotThrow(development::stop);

    assertEquals(List.of(), lines);
    assertEquals(List.of(), warnings);
  }

  @Test
  void shouldWarnWhenTheDevelopmentRunCannotBeStopped() throws Exception {
    Development development = createBuilder().build();
    try (MockedStatic<BBjShipClient> bbjShips = mockStatic(BBjShipClient.class);
        MockedStatic<DevtoolsDelivery> deliveries = mockStatic(DevtoolsDelivery.class);
        MockedStatic<Setup> setups = mockStatic(Setup.class)) {
      bbjShips.when(() -> BBjShipClient.create(URI.create(DEPLOYMENT_URL)))
          .thenReturn(bbjShipBuilder);
      deliveries.when(DevtoolsDelivery::create).thenReturn(deliveryBuilder);
      setups.when(() -> Setup.create(bundle, SNAPSHOT_VERSION)).thenReturn(setupBuilder);
      development.start();
    }

    doThrow(new IOException("BBj undeployment failed. BBjShip is not reachable")).when(bbjShip)
        .undeploy(APP_NAME);

    assertDoesNotThrow(development::stop);

    assertEquals(List.of("webforJ watch: development run of hello-app was not stopped. "
        + "BBj undeployment failed. BBjShip is not reachable"), warnings);
  }

  private Development.Builder createBuilder() {
    return Development.create(bundle, SNAPSHOT_VERSION).setDeploymentUrl(URI.create(DEPLOYMENT_URL))
        .setPublishName(APP_NAME).setCredentials(credentials).setArchive(archive)
        .setClassFolders(List.of(app.resolve(CLASS_FOLDER))).setApplicationClasspath(classpath)
        .setRuntimeClasspath(classpath).setResolver(resolver).setLog(lines::add)
        .setWarn(warnings::add).setDebug(line -> {
        });
  }
}
