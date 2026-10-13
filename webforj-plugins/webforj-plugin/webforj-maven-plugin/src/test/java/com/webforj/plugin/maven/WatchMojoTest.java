package com.webforj.plugin.maven;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.App;
import com.webforj.bundle.bun.BundlerExecution;
import com.webforj.bundle.bun.WatchSession;
import com.webforj.plugin.foundation.WatchPortFile;
import com.webforj.plugin.foundation.bbj.BBjShipClient;
import com.webforj.plugin.foundation.bbj.HelloApp;
import com.webforj.plugin.foundation.bbj.Home;
import com.webforj.plugin.foundation.bbj.Installation;
import com.webforj.plugin.foundation.bbj.ServicesProperties;
import com.webforj.plugin.foundation.bbj.Setup;
import com.webforj.plugin.foundation.bbj.model.BBjCredentials;
import com.webforj.plugin.foundation.bbj.model.JavaSettings;
import com.webforj.plugin.foundation.devtools.DevtoolsDelivery;
import com.webforj.plugin.foundation.resolve.ApplicationClasspath.ResolvedJar;
import com.webforj.plugin.maven.hotswap.HotswapInjection;
import com.webforj.plugin.maven.hotswap.HotswapOptions;
import com.webforj.plugin.maven.hotswap.jrebel.JrebelOptions;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;
import java.util.Properties;
import java.util.Set;
import java.util.function.Consumer;
import java.util.jar.JarOutputStream;
import java.util.stream.Stream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import org.apache.maven.artifact.DefaultArtifact;
import org.apache.maven.artifact.handler.DefaultArtifactHandler;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Build;
import org.apache.maven.model.Plugin;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.descriptor.MojoDescriptor;
import org.apache.maven.plugin.descriptor.PluginDescriptor;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.apache.maven.toolchain.Toolchain;
import org.apache.maven.toolchain.ToolchainManager;
import org.eclipse.aether.RepositorySystem;
import org.eclipse.aether.RepositorySystemSession;
import org.eclipse.aether.artifact.Artifact;
import org.eclipse.aether.resolution.ArtifactRequest;
import org.eclipse.aether.resolution.ArtifactResolutionException;
import org.eclipse.aether.resolution.ArtifactResult;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.Answers;
import org.mockito.ArgumentCaptor;
import org.mockito.InOrder;

class WatchMojoTest {

  private static final String APPLICATION_NAME = "hello-app";
  private static final String WEBFORJ_GROUP_ID = "com.webforj";
  private static final String WEBFORJ_VERSION = "26.03-SNAPSHOT";
  private static final String DEPLOYMENT_URL = "https://bbj.example.test/webforj-bbjship";
  private static final String COMPILE_GOAL = "compile";
  private static final String WATCH_GOAL = "webforj:watch";
  private static final String INTERRUPTION_MESSAGE = "only the wait ends with the interruption";
  private static final String CLASSES_DIRECTORY = "target/classes";
  private static final String HOTSWAP_DIRECTORY = "hotswap";
  private static final String DEVELOPMENT_PROPERTIES = "development.properties";
  private static final String CONFIGURED_CLASS_FOLDER = "/work/hello-app/target/classes";
  private static final String DEVELOPMENT_RUN_FAILURE =
      "BBj development run failed. Check the deployment settings and that ";
  private static final String DEVELOPMENT_RUNS_HINT =
      "development runs are enabled on the BBj machine";
  private static final String SERVICES_PROPERTIES_PATH = "bbx/cfg/BBj.properties";
  private static final String JBR_JAVA = "/opt/jbr/bin/java";
  private static final String JDK_JAVA = "/opt/jdk/bin/java";

  @Test
  void shouldSkipWhenThereIsNoSourceRoot() {
    ToolchainManager toolchainManager = mock(ToolchainManager.class);
    RepositorySystem repositorySystem = mock(RepositorySystem.class);
    WatchMojo mojo = new WatchMojo(toolchainManager, repositorySystem);
    mojo.project = mock(MavenProject.class);
    mojo.sourceRoot = new File("target/no-such-frontend-directory");

    assertDoesNotThrow(mojo::execute);
  }

  @Test
  void shouldOpenTheSocketAndWriteThePortFile(@TempDir Path tmp) throws Exception {
    BundlerExecution execution = mock(BundlerExecution.class);
    WatchSession session = mock(WatchSession.class);
    when(execution.watch(any(), any(), any())).thenReturn(session);
    WatchMojo mojo = newMojo(execution, tmp);
    Path portFile = WatchPortFile.resolve(tmp.toAbsolutePath().toString());

    try {
      mojo.execute();

      assertTrue(Files.exists(portFile));
      verify(execution).watch(any(), any(), any());
    } finally {
      Files.deleteIfExists(portFile);
    }
  }

  @Test
  void shouldCleanUpWhenTheInitialBuildIsInterrupted(@TempDir Path tmp) throws Exception {
    BundlerExecution execution = mock(BundlerExecution.class);
    when(execution.watch(any(), any(), any())).thenThrow(new InterruptedException("stop"));
    WatchMojo mojo = newMojo(execution, tmp);
    Path portFile = WatchPortFile.resolve(tmp.toAbsolutePath().toString());

    try {
      assertThrows(MojoExecutionException.class, mojo::execute);
      assertFalse(Files.exists(portFile));
    } finally {
      Files.deleteIfExists(portFile);
    }
  }

  @Test
  void shouldCleanUpWhenTheWatchFailsToStart(@TempDir Path tmp) throws Exception {
    BundlerExecution execution = mock(BundlerExecution.class);
    when(execution.watch(any(), any(), any())).thenThrow(new RuntimeException("boom"));
    WatchMojo mojo = newMojo(execution, tmp);
    Path portFile = WatchPortFile.resolve(tmp.toAbsolutePath().toString());

    try {
      assertThrows(MojoExecutionException.class, mojo::execute);
      assertFalse(Files.exists(portFile));
    } finally {
      Files.deleteIfExists(portFile);
    }
  }

  @Test
  void shouldKeepTheGenericLastGoalLineWithoutDevelopmentRun(@TempDir Path tmp) throws Exception {
    BundlerExecution execution = mock(BundlerExecution.class);
    WatchSession watchSession = mock(WatchSession.class);
    when(execution.watch(any(), any(), any())).thenReturn(watchSession);
    WatchMojo mojo = newMojo(execution, tmp);
    mojo.session = Development.newSession(List.of(COMPILE_GOAL, WATCH_GOAL));
    mojo.mojoExecution = Development.newWatchExecution();
    Log log = mock(Log.class);
    mojo.setLog(log);
    doAnswer(invocation -> {
      Thread.currentThread().interrupt();

      return null;
    }).when(log).info(Development.LAST_GOAL_LINE);
    Path portFile = WatchPortFile.resolve(tmp.toAbsolutePath().toString());

    try {
      mojo.execute();

      assertTrue(Thread.interrupted(), INTERRUPTION_MESSAGE);
      verify(log).info(Development.LAST_GOAL_LINE);
      verify(log, never()).info(Development.STOP_LINE);
    } finally {
      Files.deleteIfExists(portFile);
    }
  }

  @Nested
  class Development {

    static final String LAST_GOAL_LINE = "the watch is the last goal of the build and keeps it "
        + "running, stop the build to stop the watch";
    static final String ADDRESS_LINE =
        "webforJ watch: hello-app is running at https://bbj.example.test/webapp/hello-app";
    static final String STOP_LINE = "webforJ watch: press Ctrl+C to stop the app and the watch";

    @TempDir
    Path temporary;

    Path app;
    Path foundation;
    Path devtools;
    Path archive;
    Path bundle;
    Path optionsDirectory;
    Log log;
    ToolchainManager toolchainManager;
    RepositorySystem repositorySystem;
    WatchMojo mojo;
    BBjShipClient bbjShip;
    BBjShipClient.Builder bbjShipBuilder;
    DevtoolsDelivery.Builder deliveryBuilder;
    Setup setup;
    Setup.Builder setupBuilder;
    HotswapInjection injection;
    HotswapInjection.Builder injectionBuilder;

    @BeforeEach
    void createMojo() throws Exception {
      app = HelloApp.createCopy(temporary.resolve(APPLICATION_NAME));
      Files.createDirectories(app.resolve(CLASSES_DIRECTORY));
      foundation = Path.of(App.class.getProtectionDomain().getCodeSource().getLocation().toURI());
      devtools = temporary.resolve("webforj-devtools.jar");
      new JarOutputStream(Files.newOutputStream(devtools)).close();
      archive = app.resolve("target/bbj/hello-app.zip");
      DefaultArtifact framework = new DefaultArtifact(WEBFORJ_GROUP_ID, "webforj-foundation",
          WEBFORJ_VERSION, COMPILE_GOAL, "jar", null, new DefaultArtifactHandler("jar"));
      framework.setFile(foundation.toFile());
      toolchainManager = mock(ToolchainManager.class);
      repositorySystem = mock(RepositorySystem.class);
      mojo = new WatchMojo(toolchainManager, repositorySystem);
      mojo.project = new MavenProject();
      mojo.project.getBuild().setDirectory(app.resolve("target").toString());
      mojo.project.getBuild().setOutputDirectory(app.resolve(CLASSES_DIRECTORY).toString());
      mojo.project.setArtifacts(Set.of(framework));
      mojo.sourceRoot = app.resolve("src/main/frontend").toFile();
      mojo.pluginDescriptor = new PluginDescriptor();
      mojo.pluginDescriptor.setVersion(WEBFORJ_VERSION);
      mojo.bbj.setPublishName(APPLICATION_NAME);
      mojo.bbj.getDeployment().setUrl(DEPLOYMENT_URL);
      mojo.bbj.getAdmin().setUsername("administrator");
      mojo.bbj.getAdmin().setPassword("secret");
      log = mock(Log.class);
      mojo.setLog(log);
      bbjShip = mock(BBjShipClient.class);
      bbjShipBuilder = mock(BBjShipClient.Builder.class, Answers.RETURNS_SELF);
      when(bbjShipBuilder.build()).thenReturn(bbjShip);
      when(bbjShip.deploy(archive, APPLICATION_NAME))
          .thenReturn(URI.create("https://bbj.example.test/webapp/hello-app"));
      deliveryBuilder = mock(DevtoolsDelivery.Builder.class, Answers.RETURNS_SELF);
      DevtoolsDelivery delivery = mock(DevtoolsDelivery.class);
      when(deliveryBuilder.build()).thenReturn(delivery);
      when(delivery.getJars()).thenReturn(List
          .of(new ResolvedJar(WEBFORJ_GROUP_ID, "webforj-devtools", WEBFORJ_VERSION, devtools)));
      bundle = temporary.resolve("webforj-bbjship-26.03-SNAPSHOT-bbj-plugin.zip");
      Artifact bbjShipPackage = mock(Artifact.class);
      when(bbjShipPackage.getFile()).thenReturn(bundle.toFile());
      mojo.repositorySession = mock(RepositorySystemSession.class);
      when(repositorySystem.resolveArtifact(eq(mojo.repositorySession), any()))
          .thenReturn(new ArtifactResult(new ArtifactRequest()).setArtifact(bbjShipPackage));
      setup = mock(Setup.class);
      setupBuilder = mock(Setup.Builder.class, Answers.RETURNS_SELF);
      when(setupBuilder.build()).thenReturn(setup);
      optionsDirectory = temporary.resolve(HOTSWAP_DIRECTORY);
      injection = mock(HotswapInjection.class);
      when(injection.apply())
          .thenReturn(List.of("-XX:VMOptionsFile=" + optionsDirectory.resolve("hotswap.options")));
      when(injection.getContainerOptionsDirectory()).thenReturn(optionsDirectory);
      injectionBuilder = mock(HotswapInjection.Builder.class, Answers.RETURNS_SELF);
      when(injectionBuilder.isSharedContainer()).thenReturn(true);
      when(injectionBuilder.build()).thenReturn(injection);
    }

    @Test
    void shouldStartNothingWithoutTheBbjDeploymentUrl() throws Exception {
      mojo.bbj.getDeployment().setUrl("");

      try (var bbjShips = mockStatic(BBjShipClient.class);
          var deliveries = mockStatic(DevtoolsDelivery.class)) {
        mojo.execute();

        bbjShips.verifyNoInteractions();
        deliveries.verifyNoInteractions();
      }

      assertFalse(Files.exists(app.resolve("target/bbj")));
    }

    @Test
    void shouldStartTheDevelopmentRunOfTheOutputDirectoryWithTheDevtools() throws Exception {
      try (var bbjShips = mockStatic(BBjShipClient.class);
          var deliveries = mockStatic(DevtoolsDelivery.class);
          var setups = mockStatic(Setup.class)) {
        bbjShips.when(() -> BBjShipClient.create(URI.create(DEPLOYMENT_URL)))
            .thenReturn(bbjShipBuilder);
        deliveries.when(DevtoolsDelivery::create).thenReturn(deliveryBuilder);
        setups.when(() -> Setup.create(bundle, WEBFORJ_VERSION)).thenReturn(setupBuilder);

        mojo.execute();
      }

      verify(deliveryBuilder).setArtifactId("webforj-devtools");
      verify(bbjShip).deploy(archive, APPLICATION_NAME);
      try (ZipFile zip = new ZipFile(archive.toFile())) {
        assertEquals(List.of("dependencies/00000-" + foundation.getFileName(),
            "dependencies/00001-webforj-devtools.jar", DEVELOPMENT_PROPERTIES, "bbj/webforj.bbj"),
            zip.stream().map(ZipEntry::getName).toList());
        try (InputStream input = zip.getInputStream(zip.getEntry(DEVELOPMENT_PROPERTIES))) {
          String text = new String(input.readAllBytes(), StandardCharsets.UTF_8);
          assertEquals("classFolder=" + app.resolve(CLASSES_DIRECTORY).toAbsolutePath()
              + System.lineSeparator(), text.substring(text.indexOf('\n') + 1));
        }
      }

      verify(log).info(ADDRESS_LINE);
    }

    @Test
    void shouldSendTheConfiguredClassFolder() throws Exception {
      mojo.bbj.getDevelopment().setClassFolder(new File(CONFIGURED_CLASS_FOLDER));

      try (var bbjShips = mockStatic(BBjShipClient.class);
          var deliveries = mockStatic(DevtoolsDelivery.class);
          var setups = mockStatic(Setup.class)) {
        bbjShips.when(() -> BBjShipClient.create(URI.create(DEPLOYMENT_URL)))
            .thenReturn(bbjShipBuilder);
        deliveries.when(DevtoolsDelivery::create).thenReturn(deliveryBuilder);
        setups.when(() -> Setup.create(bundle, WEBFORJ_VERSION)).thenReturn(setupBuilder);

        mojo.execute();
      }

      try (ZipFile zip = new ZipFile(archive.toFile());
          InputStream input = zip.getInputStream(zip.getEntry(DEVELOPMENT_PROPERTIES))) {
        String text = new String(input.readAllBytes(), StandardCharsets.UTF_8);
        assertEquals("classFolder=" + Path.of(CONFIGURED_CLASS_FOLDER).toAbsolutePath()
            + System.lineSeparator(), text.substring(text.indexOf('\n') + 1));
      }
    }

    @Test
    void shouldFailBeforeTheSetupWhenTheClassFolderDoesNotExist() throws Exception {
      Files.delete(app.resolve(CLASSES_DIRECTORY));

      try (var bbjShips = mockStatic(BBjShipClient.class);
          var deliveries = mockStatic(DevtoolsDelivery.class);
          var setups = mockStatic(Setup.class)) {
        MojoExecutionException failure = assertThrows(MojoExecutionException.class, mojo::execute);

        assertEquals("The class folder " + app.resolve(CLASSES_DIRECTORY).toAbsolutePath()
            + " does not exist. Run compile before webforj:watch", failure.getMessage());
        bbjShips.verifyNoInteractions();
        deliveries.verifyNoInteractions();
        setups.verifyNoInteractions();
      }

      assertFalse(Files.exists(archive));
    }

    @Test
    void shouldFailTheGoalWhenTheBbjShipRefusesTheDevelopmentRun() throws Exception {
      IOException refusal = new IOException("BBj deployment failed. Development runs are not "
          + "enabled on this BBj machine. Run cli development enable");
      when(bbjShip.deploy(archive, APPLICATION_NAME)).thenThrow(refusal);

      try (var bbjShips = mockStatic(BBjShipClient.class);
          var deliveries = mockStatic(DevtoolsDelivery.class);
          var setups = mockStatic(Setup.class)) {
        bbjShips.when(() -> BBjShipClient.create(URI.create(DEPLOYMENT_URL)))
            .thenReturn(bbjShipBuilder);
        deliveries.when(DevtoolsDelivery::create).thenReturn(deliveryBuilder);
        setups.when(() -> Setup.create(bundle, WEBFORJ_VERSION)).thenReturn(setupBuilder);

        MojoExecutionException failure = assertThrows(MojoExecutionException.class, mojo::execute);

        assertEquals(DEVELOPMENT_RUN_FAILURE + DEVELOPMENT_RUNS_HINT, failure.getMessage());
        assertSame(refusal, failure.getCause());
      }
    }

    @Test
    void shouldSetUpTheBbjShipForDevelopmentRunsBeforeTheDeploy() throws Exception {
      try (var bbjShips = mockStatic(BBjShipClient.class, Answers.CALLS_REAL_METHODS);
          var deliveries = mockStatic(DevtoolsDelivery.class);
          var setups = mockStatic(Setup.class)) {
        bbjShips.when(() -> BBjShipClient.create(URI.create(DEPLOYMENT_URL)))
            .thenReturn(bbjShipBuilder);
        deliveries.when(DevtoolsDelivery::create).thenReturn(deliveryBuilder);
        setups.when(() -> Setup.create(bundle, WEBFORJ_VERSION)).thenReturn(setupBuilder);

        mojo.execute();
      }

      InOrder order = inOrder(setup, bbjShip);
      order.verify(setup).apply();
      order.verify(bbjShip).deploy(archive, APPLICATION_NAME);
      verify(setupBuilder).setDeploymentUrl(URI.create(DEPLOYMENT_URL));
      verify(setupBuilder).setPort(443);
      verify(setupBuilder).setDevelopmentEnabled(true);
      ArgumentCaptor<BBjCredentials> credentials = ArgumentCaptor.forClass(BBjCredentials.class);
      verify(setupBuilder).setCredentials(credentials.capture());
      assertEquals("administrator", credentials.getValue().getUsername());
      assertEquals("secret", credentials.getValue().getPassword());
      ArgumentCaptor<Consumer<String>> logChannel = ArgumentCaptor.captor();
      verify(setupBuilder).setLog(logChannel.capture());
      logChannel.getValue().accept("BBj setup completed");
      verify(log).info("BBj setup completed");
      ArgumentCaptor<Consumer<String>> warnChannel = ArgumentCaptor.captor();
      verify(setupBuilder).setWarn(warnChannel.capture());
      warnChannel.getValue().accept("BBjServices must be restarted");
      verify(log).warn("BBjServices must be restarted");
      verify(setupBuilder, never()).setHome(any());
      verify(setupBuilder, never()).setJavaSettings(any());
    }

    @Test
    void shouldHandTheHotswapSettingsToTheSetup() throws Exception {
      mojo.bbj.setHome(temporary.resolve("bbx").toFile());
      mojo.bbj.setProperties(temporary.resolve(SERVICES_PROPERTIES_PATH).toFile());
      mojo.session = newSession(List.of(COMPILE_GOAL, WATCH_GOAL));
      Toolchain toolchain = mock(Toolchain.class);
      Path java = temporary.resolve("jdk/bin/java");
      when(toolchain.findTool("java")).thenReturn(java.toString());
      when(toolchainManager.getToolchainFromBuildContext("jdk", mojo.session))
          .thenReturn(toolchain);

      try (var bbjShips = mockStatic(BBjShipClient.class);
          var deliveries = mockStatic(DevtoolsDelivery.class);
          var setups = mockStatic(Setup.class);
          var injections = mockStatic(HotswapInjection.class)) {
        bbjShips.when(() -> BBjShipClient.create(URI.create(DEPLOYMENT_URL)))
            .thenReturn(bbjShipBuilder);
        deliveries.when(DevtoolsDelivery::create).thenReturn(deliveryBuilder);
        setups.when(() -> Setup.create(bundle, WEBFORJ_VERSION)).thenReturn(setupBuilder);
        injections.when(HotswapInjection::create).thenReturn(injectionBuilder);

        mojo.execute();
      }

      verify(setupBuilder).setHome(temporary.resolve("bbx"));
      ArgumentCaptor<JavaSettings> settings = ArgumentCaptor.forClass(JavaSettings.class);
      verify(setupBuilder).setJavaSettings(settings.capture());
      assertEquals(Optional.of(temporary.resolve(SERVICES_PROPERTIES_PATH)),
          settings.getValue().getPropertiesFile());
      assertEquals(optionsDirectory, settings.getValue().getOptionsDirectory());
    }

    @Test
    void shouldPreferTheCommandLineHomeAndProperties() throws Exception {
      mojo.bbj.setHome(temporary.resolve("bbx").toFile());
      mojo.bbj.setProperties(temporary.resolve(SERVICES_PROPERTIES_PATH).toFile());
      mojo.home = temporary.resolve("bbx-25").toFile();
      mojo.properties = temporary.resolve("bbx-25/cfg/BBj.properties").toFile();

      try (var bbjShips = mockStatic(BBjShipClient.class);
          var deliveries = mockStatic(DevtoolsDelivery.class);
          var setups = mockStatic(Setup.class);
          var injections = mockStatic(HotswapInjection.class)) {
        bbjShips.when(() -> BBjShipClient.create(URI.create(DEPLOYMENT_URL)))
            .thenReturn(bbjShipBuilder);
        deliveries.when(DevtoolsDelivery::create).thenReturn(deliveryBuilder);
        setups.when(() -> Setup.create(bundle, WEBFORJ_VERSION)).thenReturn(setupBuilder);
        injections.when(HotswapInjection::create).thenReturn(injectionBuilder);

        mojo.execute();
      }

      verify(setupBuilder).setHome(temporary.resolve("bbx-25"));
      ArgumentCaptor<JavaSettings> settings = ArgumentCaptor.forClass(JavaSettings.class);
      verify(setupBuilder).setJavaSettings(settings.capture());
      assertEquals(Optional.of(temporary.resolve("bbx-25/cfg/BBj.properties")),
          settings.getValue().getPropertiesFile());
    }

    @Test
    void shouldLeaveThePropertiesFileOfTheHomeToTheSetupWithoutToolchain() throws Exception {
      try (var bbjShips = mockStatic(BBjShipClient.class);
          var deliveries = mockStatic(DevtoolsDelivery.class);
          var setups = mockStatic(Setup.class);
          var injections = mockStatic(HotswapInjection.class)) {
        bbjShips.when(() -> BBjShipClient.create(URI.create(DEPLOYMENT_URL)))
            .thenReturn(bbjShipBuilder);
        deliveries.when(DevtoolsDelivery::create).thenReturn(deliveryBuilder);
        setups.when(() -> Setup.create(bundle, WEBFORJ_VERSION)).thenReturn(setupBuilder);
        injections.when(HotswapInjection::create).thenReturn(injectionBuilder);

        mojo.execute();
      }

      ArgumentCaptor<JavaSettings> settings = ArgumentCaptor.forClass(JavaSettings.class);
      verify(setupBuilder).setJavaSettings(settings.capture());
      assertEquals(Optional.empty(), settings.getValue().getPropertiesFile());
      assertEquals(optionsDirectory, settings.getValue().getOptionsDirectory());
    }

    @ParameterizedTest
    @MethodSource("getServicesProperties")
    void shouldHandTheJavaOfBbjServicesToTheHotswapCheckOfTheSharedContainer(String properties,
        Path java) throws Exception {
      Path bbx = Home.createCopy(temporary.resolve("bbx"));
      Files.writeString(bbx.resolve("cfg/BBj.properties"), properties);
      mojo.bbj.setHome(bbx.toFile());
      mojo.hotswap = new HotswapOptions();

      try (var bbjShips = mockStatic(BBjShipClient.class);
          var deliveries = mockStatic(DevtoolsDelivery.class);
          var setups = mockStatic(Setup.class);
          var injections = mockStatic(HotswapInjection.class)) {
        bbjShips.when(() -> BBjShipClient.create(URI.create(DEPLOYMENT_URL)))
            .thenReturn(bbjShipBuilder);
        deliveries.when(DevtoolsDelivery::create).thenReturn(deliveryBuilder);
        setups.when(() -> Setup.create(bundle, WEBFORJ_VERSION)).thenReturn(setupBuilder);
        injections.when(HotswapInjection::create).thenReturn(injectionBuilder);

        mojo.execute();
      }

      verify(injectionBuilder).setJavaExecutable(java);
    }

    @Test
    void shouldLoadThePropertiesFileOfBbjServicesFromTheHome() throws Exception {
      Path bbx = Home.createCopy(temporary.resolve("bbx")).toRealPath();
      Files.writeString(bbx.resolve("cfg/BBj.properties"), """
          basis.java.jvm.BBjServices=/opt/jbr/bin/java
          """);
      mojo.bbj.setHome(bbx.toFile());
      mojo.hotswap = new HotswapOptions();

      try (var bbjShips = mockStatic(BBjShipClient.class);
          var deliveries = mockStatic(DevtoolsDelivery.class);
          var setups = mockStatic(Setup.class);
          var injections = mockStatic(HotswapInjection.class);
          var services = mockStatic(ServicesProperties.class, Answers.CALLS_REAL_METHODS)) {
        bbjShips.when(() -> BBjShipClient.create(URI.create(DEPLOYMENT_URL)))
            .thenReturn(bbjShipBuilder);
        deliveries.when(DevtoolsDelivery::create).thenReturn(deliveryBuilder);
        setups.when(() -> Setup.create(bundle, WEBFORJ_VERSION)).thenReturn(setupBuilder);
        injections.when(HotswapInjection::create).thenReturn(injectionBuilder);

        mojo.execute();

        services.verify(() -> ServicesProperties.findJavaExecutable(bbx, null));
      }

      verify(injectionBuilder).setJavaExecutable(Path.of(JBR_JAVA));
    }

    @ParameterizedTest
    @MethodSource("getSelections")
    void shouldHandTheJavaOfBbjServicesOnlyWhenTheCommandLineSelectsHotswap(String selection,
        Path java) throws Exception {
      Path file = temporary.resolve("BBj.properties");
      Files.writeString(file, """
          basis.java.jvm.BBjServices=/opt/jbr/bin/java
          """);
      mojo.properties = file.toFile();
      mojo.hotswapSelection = selection;
      mojo.session = newSession(List.of(COMPILE_GOAL, WATCH_GOAL));
      Toolchain toolchain = mock(Toolchain.class);
      when(toolchain.findTool("java")).thenReturn(JDK_JAVA);
      when(toolchainManager.getToolchainFromBuildContext("jdk", mojo.session))
          .thenReturn(toolchain);

      try (var bbjShips = mockStatic(BBjShipClient.class);
          var deliveries = mockStatic(DevtoolsDelivery.class);
          var setups = mockStatic(Setup.class);
          var installations = mockStatic(Installation.class);
          var injections = mockStatic(HotswapInjection.class)) {
        bbjShips.when(() -> BBjShipClient.create(URI.create(DEPLOYMENT_URL)))
            .thenReturn(bbjShipBuilder);
        deliveries.when(DevtoolsDelivery::create).thenReturn(deliveryBuilder);
        setups.when(() -> Setup.create(bundle, WEBFORJ_VERSION)).thenReturn(setupBuilder);
        installations.when(() -> Installation.resolveHome(null)).thenReturn(temporary);
        injections.when(HotswapInjection::create).thenReturn(injectionBuilder);

        mojo.execute();
      }

      verify(injectionBuilder).setJavaExecutable(java);
    }

    @Test
    void shouldHandNoJavaToTheHotswapCheckWithoutThePropertiesFileOfBbjServices() throws Exception {
      mojo.bbj.setProperties(temporary.resolve(SERVICES_PROPERTIES_PATH).toFile());
      mojo.hotswap = new HotswapOptions();

      try (var bbjShips = mockStatic(BBjShipClient.class);
          var deliveries = mockStatic(DevtoolsDelivery.class);
          var setups = mockStatic(Setup.class);
          var injections = mockStatic(HotswapInjection.class)) {
        bbjShips.when(() -> BBjShipClient.create(URI.create(DEPLOYMENT_URL)))
            .thenReturn(bbjShipBuilder);
        deliveries.when(DevtoolsDelivery::create).thenReturn(deliveryBuilder);
        setups.when(() -> Setup.create(bundle, WEBFORJ_VERSION)).thenReturn(setupBuilder);
        injections.when(HotswapInjection::create).thenReturn(injectionBuilder);

        mojo.execute();
      }

      verify(injectionBuilder).setJavaExecutable(null);
    }

    @Test
    void shouldFailTheGoalWhenTheHomeOfBbjServicesCannotBeResolved() throws Exception {
      IOException missing =
          new IOException("Cannot detect a BBj installation. Configure bbj.home or BBJ_HOME");
      mojo.hotswap = new HotswapOptions();

      try (var installations = mockStatic(Installation.class);
          var injections = mockStatic(HotswapInjection.class)) {
        installations.when(() -> Installation.resolveHome(null)).thenThrow(missing);
        injections.when(HotswapInjection::create).thenReturn(injectionBuilder);

        MojoExecutionException failure = assertThrows(MojoExecutionException.class, mojo::execute);

        assertEquals("Cannot detect a BBj installation. Configure bbj.home or BBJ_HOME",
            failure.getMessage());
        assertSame(missing, failure.getCause());
      }

      verify(injection, never()).apply();
    }

    @Test
    void shouldHandTheCommandLineHotswapDirectoryToTheInjection() throws Exception {
      mojo.hotswapDirectory = temporary.resolve(HOTSWAP_DIRECTORY).toFile();

      try (var bbjShips = mockStatic(BBjShipClient.class);
          var deliveries = mockStatic(DevtoolsDelivery.class);
          var setups = mockStatic(Setup.class);
          var injections = mockStatic(HotswapInjection.class)) {
        bbjShips.when(() -> BBjShipClient.create(URI.create(DEPLOYMENT_URL)))
            .thenReturn(bbjShipBuilder);
        deliveries.when(DevtoolsDelivery::create).thenReturn(deliveryBuilder);
        setups.when(() -> Setup.create(bundle, WEBFORJ_VERSION)).thenReturn(setupBuilder);
        injections.when(HotswapInjection::create).thenReturn(injectionBuilder);

        mojo.execute();
      }

      verify(injectionBuilder).setDirectory(temporary.resolve(HOTSWAP_DIRECTORY));
    }

    @Test
    void shouldHandTheToolchainJavaToTheHotswapCheckOfProjectWithRunner() throws Exception {
      Plugin runner = new Plugin();
      runner.setArtifactId("spring-boot-maven-plugin");
      mojo.project.getBuild().addPlugin(runner);
      mojo.hotswap = new HotswapOptions();
      Path java = temporary.resolve("jdk/bin/java");
      mojo.session = newSession(List.of(COMPILE_GOAL, WATCH_GOAL));
      Toolchain toolchain = mock(Toolchain.class);
      when(toolchain.findTool("java")).thenReturn(java.toString());
      when(toolchainManager.getToolchainFromBuildContext("jdk", mojo.session))
          .thenReturn(toolchain);
      when(injectionBuilder.isSharedContainer()).thenReturn(false);

      try (var bbjShips = mockStatic(BBjShipClient.class);
          var deliveries = mockStatic(DevtoolsDelivery.class);
          var setups = mockStatic(Setup.class);
          var injections = mockStatic(HotswapInjection.class)) {
        bbjShips.when(() -> BBjShipClient.create(URI.create(DEPLOYMENT_URL)))
            .thenReturn(bbjShipBuilder);
        deliveries.when(DevtoolsDelivery::create).thenReturn(deliveryBuilder);
        setups.when(() -> Setup.create(bundle, WEBFORJ_VERSION)).thenReturn(setupBuilder);
        injections.when(HotswapInjection::create).thenReturn(injectionBuilder);

        mojo.execute();
      }

      verify(injectionBuilder).setJavaExecutable(java);
    }

    @Test
    void shouldKeepTheJavaSettingsOfBbjServicesForProjectWithRunner() throws Exception {
      Plugin runner = new Plugin();
      runner.setArtifactId("spring-boot-maven-plugin");
      mojo.project.getBuild().addPlugin(runner);
      Path library = Files.createFile(temporary.resolve("libjrebel64.dylib"));
      mojo.hotswap = new HotswapOptions().setJrebel(new JrebelOptions().setPath(library.toFile()));

      try (var bbjShips = mockStatic(BBjShipClient.class);
          var deliveries = mockStatic(DevtoolsDelivery.class);
          var setups = mockStatic(Setup.class)) {
        bbjShips.when(() -> BBjShipClient.create(URI.create(DEPLOYMENT_URL)))
            .thenReturn(bbjShipBuilder);
        deliveries.when(DevtoolsDelivery::create).thenReturn(deliveryBuilder);
        setups.when(() -> Setup.create(bundle, WEBFORJ_VERSION)).thenReturn(setupBuilder);

        mojo.execute();
      }

      assertTrue(mojo.project.getProperties().getProperty("spring-boot.run.jvmArguments")
          .contains("-agentpath:" + library));
      verify(setupBuilder, never()).setJavaSettings(any());
      verify(setup).apply();
    }

    @Test
    void shouldLeaveTheTokenToTheSetup() throws Exception {
      mojo.bbj.getAdmin().setToken("4f2a9c");

      try (var bbjShips = mockStatic(BBjShipClient.class);
          var deliveries = mockStatic(DevtoolsDelivery.class);
          var setups = mockStatic(Setup.class)) {
        bbjShips.when(() -> BBjShipClient.create(URI.create(DEPLOYMENT_URL)))
            .thenReturn(bbjShipBuilder);
        deliveries.when(DevtoolsDelivery::create).thenReturn(deliveryBuilder);
        setups.when(() -> Setup.create(bundle, WEBFORJ_VERSION)).thenReturn(setupBuilder);

        mojo.execute();
      }

      ArgumentCaptor<BBjCredentials> credentials = ArgumentCaptor.forClass(BBjCredentials.class);
      verify(setupBuilder).setCredentials(credentials.capture());
      assertTrue(credentials.getValue().isToken());
    }

    @Test
    void shouldFailTheGoalWhenTheSetupFails() throws Exception {
      IOException refusal = new IOException("BBj setup is local only. Run setup on the BBj host");
      doThrow(refusal).when(setup).apply();

      try (var bbjShips = mockStatic(BBjShipClient.class);
          var deliveries = mockStatic(DevtoolsDelivery.class);
          var setups = mockStatic(Setup.class)) {
        bbjShips.when(() -> BBjShipClient.create(URI.create(DEPLOYMENT_URL)))
            .thenReturn(bbjShipBuilder);
        deliveries.when(DevtoolsDelivery::create).thenReturn(deliveryBuilder);
        setups.when(() -> Setup.create(bundle, WEBFORJ_VERSION)).thenReturn(setupBuilder);

        MojoExecutionException failure = assertThrows(MojoExecutionException.class, mojo::execute);

        assertEquals(DEVELOPMENT_RUN_FAILURE + DEVELOPMENT_RUNS_HINT, failure.getMessage());
        assertSame(refusal, failure.getCause());
      }

      verify(bbjShip, never()).deploy(any(), any());
    }

    @Test
    void shouldFailTheGoalWhenTheBbjShipPackageCannotBeResolved() throws Exception {
      ArtifactResolutionException missing = new ArtifactResolutionException(List.of());
      when(repositorySystem.resolveArtifact(eq(mojo.repositorySession), any())).thenThrow(missing);

      MojoExecutionException failure = assertThrows(MojoExecutionException.class, mojo::execute);

      assertEquals(DEVELOPMENT_RUN_FAILURE + DEVELOPMENT_RUNS_HINT, failure.getMessage());
      assertSame(missing, failure.getCause());
    }

    @Test
    void shouldStopTheDevelopmentRun() throws Exception {
      Optional<WatchMojo.DevelopmentRun> run;
      try (var bbjShips = mockStatic(BBjShipClient.class);
          var deliveries = mockStatic(DevtoolsDelivery.class);
          var setups = mockStatic(Setup.class)) {
        bbjShips.when(() -> BBjShipClient.create(URI.create(DEPLOYMENT_URL)))
            .thenReturn(bbjShipBuilder);
        deliveries.when(DevtoolsDelivery::create).thenReturn(deliveryBuilder);
        setups.when(() -> Setup.create(bundle, WEBFORJ_VERSION)).thenReturn(setupBuilder);
        run = mojo.startDevelopmentRun();
      }

      mojo.stopDevelopmentRun(run.orElseThrow());

      verify(bbjShip).undeploy(APPLICATION_NAME);
      verify(log).info("webforJ watch: development run of hello-app stopped");
    }

    @Test
    void shouldWarnWhenTheDevelopmentRunCannotBeStopped() throws Exception {
      Optional<WatchMojo.DevelopmentRun> run;
      try (var bbjShips = mockStatic(BBjShipClient.class);
          var deliveries = mockStatic(DevtoolsDelivery.class);
          var setups = mockStatic(Setup.class)) {
        bbjShips.when(() -> BBjShipClient.create(URI.create(DEPLOYMENT_URL)))
            .thenReturn(bbjShipBuilder);
        deliveries.when(DevtoolsDelivery::create).thenReturn(deliveryBuilder);
        setups.when(() -> Setup.create(bundle, WEBFORJ_VERSION)).thenReturn(setupBuilder);
        run = mojo.startDevelopmentRun();
      }

      doThrow(new IOException("BBj undeployment failed. BBjShip is not reachable")).when(bbjShip)
          .undeploy(APPLICATION_NAME);

      assertDoesNotThrow(() -> mojo.stopDevelopmentRun(run.orElseThrow()));

      verify(log).warn("webforJ watch: development run of hello-app was not stopped. "
          + "BBj undeployment failed. BBjShip is not reachable");
    }

    @ParameterizedTest
    @MethodSource("getGoals")
    void shouldKeepTheBuildRunningOnlyWhileTheWatchIsTheLastGoal(List<String> goals,
        String defaultGoal, boolean kept) throws Exception {
      mojo.session = newSession(goals);
      MavenProject topLevel = new MavenProject();
      topLevel.getBuild().setDefaultGoal(defaultGoal);
      when(mojo.session.getTopLevelProject()).thenReturn(topLevel);
      mojo.mojoExecution = newWatchExecution();
      doAnswer(invocation -> {
        Thread.currentThread().interrupt();

        return null;
      }).when(log).info(STOP_LINE);

      try (var bbjShips = mockStatic(BBjShipClient.class);
          var deliveries = mockStatic(DevtoolsDelivery.class);
          var setups = mockStatic(Setup.class)) {
        bbjShips.when(() -> BBjShipClient.create(URI.create(DEPLOYMENT_URL)))
            .thenReturn(bbjShipBuilder);
        deliveries.when(DevtoolsDelivery::create).thenReturn(deliveryBuilder);
        setups.when(() -> Setup.create(bundle, WEBFORJ_VERSION)).thenReturn(setupBuilder);

        mojo.execute();
      }

      assertEquals(kept, Thread.interrupted(), INTERRUPTION_MESSAGE);
      verify(log, times(kept ? 1 : 0)).info(STOP_LINE);
    }

    @ParameterizedTest
    @MethodSource("getAddressLines")
    void shouldPrintTheAddressOfTheDevelopmentRunAsTheLastLines(List<String> goals,
        List<String> expected) throws Exception {
      mojo.session = newSession(goals);
      mojo.mojoExecution = newWatchExecution();
      doAnswer(invocation -> {
        Thread.currentThread().interrupt();

        return null;
      }).when(log).info(STOP_LINE);

      try (var bbjShips = mockStatic(BBjShipClient.class);
          var deliveries = mockStatic(DevtoolsDelivery.class);
          var setups = mockStatic(Setup.class)) {
        bbjShips.when(() -> BBjShipClient.create(URI.create(DEPLOYMENT_URL)))
            .thenReturn(bbjShipBuilder);
        deliveries.when(DevtoolsDelivery::create).thenReturn(deliveryBuilder);
        setups.when(() -> Setup.create(bundle, WEBFORJ_VERSION)).thenReturn(setupBuilder);

        mojo.execute();
      }

      assertEquals(expected.contains(STOP_LINE), Thread.interrupted(), INTERRUPTION_MESSAGE);
      ArgumentCaptor<CharSequence> lines = ArgumentCaptor.forClass(CharSequence.class);
      verify(log, atLeastOnce()).info(lines.capture());
      List<CharSequence> logged = lines.getAllValues();
      assertEquals(expected, logged.subList(logged.size() - expected.size(), logged.size()));
      verify(log, never()).info(LAST_GOAL_LINE);
    }

    @Test
    void shouldReturnAtOnceWhenTheGoalHadNothingToDo() throws Exception {
      mojo.bbj.getDeployment().setUrl("");
      mojo.session = newSession(List.of(COMPILE_GOAL, WATCH_GOAL));
      mojo.mojoExecution = newWatchExecution();

      mojo.execute();

      verify(log, never()).info(LAST_GOAL_LINE);
    }

    private static Stream<Arguments> getGoals() {
      return Stream.of(Arguments.of(List.of(COMPILE_GOAL, WATCH_GOAL), null, true),
          Arguments.of(List.of("com.webforj:webforj-maven-plugin:watch"), null, true),
          Arguments.of(List.of("com.webforj:webforj-maven-plugin:26.03:watch"), null, true),
          Arguments.of(List.of("webforj:watch@development"), null, true),
          Arguments.of(List.of(), "compile webforj:watch", true),
          Arguments.of(List.of(WATCH_GOAL, "jetty:run"), null, false),
          Arguments.of(List.of(WATCH_GOAL, "spring-boot:run"), null, false),
          Arguments.of(List.of("other:watch"), null, false),
          Arguments.of(List.of("org.example:webforj-maven-plugin:watch"), null, false),
          Arguments.of(List.of("watch"), null, false), Arguments.of(List.of(), null, false));
    }

    private static Stream<Arguments> getAddressLines() {
      return Stream.of(
          Arguments.of(List.of(COMPILE_GOAL, WATCH_GOAL), List.of(ADDRESS_LINE, STOP_LINE)),
          Arguments.of(List.of(WATCH_GOAL, "jetty:run"), List.of(ADDRESS_LINE)));
    }

    private static Stream<Arguments> getServicesProperties() {
      return Stream.of(Arguments.of("""
          basis.java.jvm.BBjServices=/opt/jbr/bin/java
          basis.java.jvm.Default=/opt/zulu/bin/java
          """, Path.of(JBR_JAVA)), Arguments.of("""
          basis.java.jvm.Default=/opt/zulu/bin/java
          """, Path.of("/opt/zulu/bin/java")), Arguments.of("""
          basis.cacheDirectory=/opt/bbx/cache
          """, null));
    }

    private static Stream<Arguments> getSelections() {
      return Stream.of(Arguments.of("hotswapAgent", Path.of(JBR_JAVA)),
          Arguments.of("", Path.of(JDK_JAVA)), Arguments.of(null, Path.of(JDK_JAVA)));
    }

    private static MavenSession newSession(List<String> goals) {
      MavenSession session = mock(MavenSession.class);
      when(session.getGoals()).thenReturn(goals);
      when(session.getUserProperties()).thenReturn(new Properties());

      return session;
    }

    private static MojoExecution newWatchExecution() {
      PluginDescriptor plugin = new PluginDescriptor();
      plugin.setGroupId(WEBFORJ_GROUP_ID);
      plugin.setArtifactId("webforj-maven-plugin");
      plugin.setGoalPrefix("webforj");
      MojoDescriptor descriptor = new MojoDescriptor();
      descriptor.setGoal("watch");
      descriptor.setPluginDescriptor(plugin);

      return new MojoExecution(descriptor);
    }
  }

  private static WatchMojo newMojo(BundlerExecution execution, Path tmp) throws Exception {
    final Path frontend = Files.createDirectories(tmp.resolve("src/main/frontend"));
    Path classes = Files.createDirectories(tmp.resolve(CLASSES_DIRECTORY));

    Build build = new Build();
    build.setOutputDirectory(classes.toString());
    MavenProject project = mock(MavenProject.class);
    when(project.getBuild()).thenReturn(build);
    when(project.getBasedir()).thenReturn(tmp.toFile());
    when(project.getArtifactId()).thenReturn("app");
    when(project.getArtifacts()).thenReturn(Set.of());

    ToolchainManager toolchainManager = mock(ToolchainManager.class);
    RepositorySystem repositorySystem = mock(RepositorySystem.class);
    WatchMojo mojo = new WatchMojo(toolchainManager, repositorySystem) {
      @Override
      protected BundlerExecution createExecution() {
        return execution;
      }
    };
    mojo.project = project;
    mojo.sourceRoot = frontend.toFile();
    mojo.workDir = tmp.resolve("target/bundle").toFile();

    return mojo;
  }
}
