package com.webforj.plugin.maven;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import com.webforj.bbjship.foundation.Setup;
import com.webforj.bbjship.foundation.model.BBjCredentials;
import com.webforj.plugin.maven.bbj.model.BBjAdminOptions;
import com.webforj.plugin.maven.bbj.model.BBjOptions;
import java.io.IOException;
import java.net.URI;
import java.nio.file.Path;
import java.util.List;
import java.util.function.Consumer;
import java.util.stream.Stream;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.descriptor.PluginDescriptor;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.eclipse.aether.RepositorySystem;
import org.eclipse.aether.RepositorySystemSession;
import org.eclipse.aether.artifact.DefaultArtifact;
import org.eclipse.aether.repository.RemoteRepository;
import org.eclipse.aether.resolution.ArtifactRequest;
import org.eclipse.aether.resolution.ArtifactResolutionException;
import org.eclipse.aether.resolution.ArtifactResult;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.Answers;
import org.mockito.ArgumentCaptor;

class BBjSetupMojoTest {

  private static final String WEBFORJ_VERSION = "26.03-SNAPSHOT";
  private static final String LOCAL_DEPLOYMENT_URL = "http://localhost:8888/webforj-bbjship";
  private static final String ADMIN_USERNAME = "administrator";
  private static final String ADMIN_PASSWORD = "secret";
  private static final String SECURE_DEPLOYMENT_URL = "https://localhost/webforj-bbjship";
  private static final String SETUP_FAILED_MESSAGE =
      "BBj setup failed. Check the BBj plugin artifact and setup settings";

  @TempDir
  Path temporary;

  Path bundle;
  Log log;
  RepositorySystem repositorySystem;
  BBjSetupMojo mojo;
  Setup setup;
  Setup.Builder setupBuilder;

  @BeforeEach
  void createMojo() throws Exception {
    repositorySystem = mock(RepositorySystem.class);
    mojo = new BBjSetupMojo(repositorySystem);
    mojo.project = mock(MavenProject.class);
    when(mojo.project.getRemoteProjectRepositories()).thenReturn(List.of(
        new RemoteRepository.Builder("releases", "default", "https://repo.example.test/releases")
            .build()));
    mojo.pluginDescriptor = new PluginDescriptor();
    mojo.pluginDescriptor.setVersion(WEBFORJ_VERSION);
    mojo.bbj.setHome(temporary.resolve("bbx").toFile());
    mojo.bbj.getDeployment().setUrl(LOCAL_DEPLOYMENT_URL);
    mojo.bbj.getAdmin().setUsername(ADMIN_USERNAME);
    mojo.bbj.getAdmin().setPassword(ADMIN_PASSWORD);
    log = mock(Log.class);
    mojo.setLog(log);
    bundle = temporary.resolve("webforj-bbjship-plugin-26.03-SNAPSHOT-bbj-plugin.zip");
    mojo.repositorySession = mock(RepositorySystemSession.class);
    when(repositorySystem.resolveArtifact(eq(mojo.repositorySession), any()))
        .thenReturn(new ArtifactResult(new ArtifactRequest()).setArtifact(
            new DefaultArtifact("com.webforj:webforj-bbjship-plugin:zip:bbj-plugin:26.03-SNAPSHOT")
                .setFile(bundle.toFile())));
    setup = mock(Setup.class);
    setupBuilder = mock(Setup.Builder.class, Answers.RETURNS_SELF);
    when(setupBuilder.build()).thenReturn(setup);
  }

  @Test
  void shouldApplyTheResolvedBbjShipPluginWithTheConfiguredSettings() throws Exception {
    mojo.force = true;

    try (var setups = mockStatic(Setup.class)) {
      setups.when(() -> Setup.create(bundle, WEBFORJ_VERSION)).thenReturn(setupBuilder);

      mojo.execute();
    }

    ArgumentCaptor<ArtifactRequest> request = ArgumentCaptor.forClass(ArtifactRequest.class);
    verify(repositorySystem).resolveArtifact(eq(mojo.repositorySession), request.capture());
    assertEquals("com.webforj:webforj-bbjship-plugin:zip:bbj-plugin:26.03-SNAPSHOT",
        request.getValue().getArtifact().toString());
    assertEquals("https://repo.example.test/releases",
        request.getValue().getRepositories().get(0).getUrl());
    verify(setupBuilder).setHome(temporary.resolve("bbx"));
    verify(setupBuilder).setDeploymentUrl(URI.create(LOCAL_DEPLOYMENT_URL));
    verify(setupBuilder).setPort(8888);
    verify(setupBuilder).setForce(true);
    ArgumentCaptor<BBjCredentials> credentials = ArgumentCaptor.forClass(BBjCredentials.class);
    verify(setupBuilder).setCredentials(credentials.capture());
    assertEquals(ADMIN_USERNAME, credentials.getValue().getUsername());
    assertEquals(ADMIN_PASSWORD, credentials.getValue().getPassword());
    ArgumentCaptor<Consumer<String>> logChannel = ArgumentCaptor.captor();
    verify(setupBuilder).setLog(logChannel.capture());
    logChannel.getValue().accept("Installed the webforJ plugin");
    verify(log).info("Installed the webforJ plugin");
    ArgumentCaptor<Consumer<String>> warnChannel = ArgumentCaptor.captor();
    verify(setupBuilder).setWarn(warnChannel.capture());
    warnChannel.getValue().accept("BBjServices must be restarted");
    verify(log).warn("BBjServices must be restarted");
    verify(setup).apply();
  }

  @Test
  void shouldPreferTheCommandLineOverTheConfiguration() throws Exception {
    mojo.home = temporary.resolve("bbx-25").toFile();
    mojo.deploymentUrl = SECURE_DEPLOYMENT_URL;
    mojo.username = "installer";
    mojo.password = "installer-secret";

    try (var setups = mockStatic(Setup.class)) {
      setups.when(() -> Setup.create(bundle, WEBFORJ_VERSION)).thenReturn(setupBuilder);

      mojo.execute();
    }

    verify(setupBuilder).setHome(temporary.resolve("bbx-25"));
    verify(setupBuilder).setDeploymentUrl(URI.create(SECURE_DEPLOYMENT_URL));
    ArgumentCaptor<BBjCredentials> credentials = ArgumentCaptor.forClass(BBjCredentials.class);
    verify(setupBuilder).setCredentials(credentials.capture());
    assertEquals("installer", credentials.getValue().getUsername());
    assertEquals("installer-secret", credentials.getValue().getPassword());
  }

  @Test
  void shouldLeaveTheHomeToSetupWhenNoneIsConfigured() throws Exception {
    mojo.bbj = new BBjOptions();
    mojo.bbj.getDeployment().setUrl(LOCAL_DEPLOYMENT_URL);

    try (var setups = mockStatic(Setup.class)) {
      setups.when(() -> Setup.create(bundle, WEBFORJ_VERSION)).thenReturn(setupBuilder);

      mojo.execute();
    }

    verify(setupBuilder, never()).setHome(any());
    verify(setup).apply();
  }

  @ParameterizedTest
  @MethodSource("getPorts")
  void shouldPassTheDefaultPortOfTheUrlScheme(String url, int port) throws Exception {
    mojo.bbj.getDeployment().setUrl(url);

    try (var setups = mockStatic(Setup.class)) {
      setups.when(() -> Setup.create(bundle, WEBFORJ_VERSION)).thenReturn(setupBuilder);

      mojo.execute();
    }

    verify(setupBuilder).setPort(port);
  }

  @Test
  void shouldRefuseToSetUpWithoutUrl() {
    mojo.bbj.getDeployment().setUrl("");

    MojoExecutionException failure = assertThrows(MojoExecutionException.class, mojo::execute);

    assertEquals("BBj deployment requires bbj.deployment.url. Set it in plugin configuration or "
        + "pass -Dwebforj.bbj.deployment.url=<URL>", failure.getMessage());
    verifyNoInteractions(repositorySystem);
  }

  @Test
  void shouldRejectAnInvalidUrlBeforeResolvingTheBbjShipPlugin() {
    mojo.deploymentUrl = "https://localhost/%invalid";

    MojoExecutionException failure = assertThrows(MojoExecutionException.class, mojo::execute);

    assertEquals(SETUP_FAILED_MESSAGE, failure.getMessage());
    verifyNoInteractions(repositorySystem);
  }

  @Test
  void shouldReportThatTheBbjShipPluginCannotBeResolved() throws Exception {
    ArtifactResolutionException missing = new ArtifactResolutionException(List.of());
    when(repositorySystem.resolveArtifact(eq(mojo.repositorySession), any())).thenThrow(missing);

    MojoExecutionException failure = assertThrows(MojoExecutionException.class, mojo::execute);

    assertEquals(SETUP_FAILED_MESSAGE, failure.getMessage());
    assertSame(missing, failure.getCause());
  }

  @Test
  void shouldSetUpWithThePasswordWhenTheTokenIsConfiguredToo() throws Exception {
    mojo.bbj.getAdmin().setUsername(ADMIN_USERNAME);
    mojo.bbj.getAdmin().setPassword(ADMIN_PASSWORD);
    mojo.bbj.getAdmin().setToken("4f2a9c");

    try (var setups = mockStatic(Setup.class)) {
      setups.when(() -> Setup.create(bundle, WEBFORJ_VERSION)).thenReturn(setupBuilder);

      mojo.execute();
    }

    ArgumentCaptor<BBjCredentials> credentials = ArgumentCaptor.forClass(BBjCredentials.class);
    verify(setupBuilder).setCredentials(credentials.capture());
    assertFalse(credentials.getValue().isToken());
    assertEquals(ADMIN_USERNAME, credentials.getValue().getUsername());
    assertEquals(ADMIN_PASSWORD, credentials.getValue().getPassword());
  }

  @ParameterizedTest
  @MethodSource("getSetupFailures")
  void shouldReportTheFailureOfTheSetup(Exception setupFailure) throws Exception {
    doThrow(setupFailure).when(setup).apply();

    try (var setups = mockStatic(Setup.class)) {
      setups.when(() -> Setup.create(bundle, WEBFORJ_VERSION)).thenReturn(setupBuilder);

      MojoExecutionException failure = assertThrows(MojoExecutionException.class, mojo::execute);

      assertEquals(SETUP_FAILED_MESSAGE, failure.getMessage());
      assertSame(setupFailure, failure.getCause());
    }
  }

  @ParameterizedTest
  @MethodSource("getAccounts")
  void shouldWarnWhileTheFactoryAccountIsInUse(String username, String password, int warnings)
      throws Exception {
    mojo.bbj.setAdmin(new BBjAdminOptions());
    mojo.username = username;
    mojo.password = password;

    try (var setups = mockStatic(Setup.class)) {
      setups.when(() -> Setup.create(bundle, WEBFORJ_VERSION)).thenReturn(setupBuilder);

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

    assertEquals(missing + ". Configure bbj.admin.username and bbj.admin.password",
        failure.getMessage());
  }

  private static Stream<Arguments> getAccounts() {
    return Stream.of(Arguments.of(null, null, 1), Arguments.of("admin", "admin123", 1),
        Arguments.of("admin", ADMIN_PASSWORD, 0));
  }

  private static Stream<Arguments> getIncompleteCredentials() {
    return Stream.of(
        Arguments.of(null, ADMIN_PASSWORD, "The BBj administration user name is missing"),
        Arguments.of("deployer", null, "The BBj administration password is missing"));
  }

  private static Stream<Arguments> getPorts() {
    return Stream.of(Arguments.of(SECURE_DEPLOYMENT_URL, 443),
        Arguments.of("http://localhost/webforj-bbjship", 80));
  }

  private static Stream<Arguments> getSetupFailures() {
    return Stream.of(Arguments.of(new IOException("Missing BBj administration credentials")),
        Arguments.of(new IllegalStateException("BBj installation is not writable")));
  }
}
