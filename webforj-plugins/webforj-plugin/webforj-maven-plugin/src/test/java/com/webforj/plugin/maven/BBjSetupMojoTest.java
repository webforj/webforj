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

import com.webforj.plugin.foundation.bbj.Setup;
import com.webforj.plugin.foundation.bbj.model.BBjCredentials;
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
    mojo.pluginDescriptor.setVersion("26.03-SNAPSHOT");
    mojo.bbj.setHome(temporary.resolve("bbx").toFile());
    mojo.bbj.getDeployment().setUrl("http://localhost:8888/webforj-bbjship");
    mojo.bbj.getAdmin().setUsername("administrator");
    mojo.bbj.getAdmin().setPassword("secret");
    log = mock(Log.class);
    mojo.setLog(log);
    bundle = temporary.resolve("webforj-bbjship-26.03-SNAPSHOT-bbj-plugin.zip");
    mojo.repositorySession = mock(RepositorySystemSession.class);
    when(repositorySystem.resolveArtifact(eq(mojo.repositorySession), any()))
        .thenReturn(new ArtifactResult(new ArtifactRequest()).setArtifact(
            new DefaultArtifact("com.webforj:webforj-bbjship:zip:bbj-plugin:26.03-SNAPSHOT")
                .setFile(bundle.toFile())));
    setup = mock(Setup.class);
    setupBuilder = mock(Setup.Builder.class, Answers.RETURNS_SELF);
    when(setupBuilder.build()).thenReturn(setup);
  }

  @Test
  void shouldApplyTheResolvedBbjShipPluginWithTheConfiguredSettings() throws Exception {
    mojo.force = true;

    try (var setups = mockStatic(Setup.class)) {
      setups.when(() -> Setup.create(bundle, "26.03-SNAPSHOT")).thenReturn(setupBuilder);

      mojo.execute();
    }

    ArgumentCaptor<ArtifactRequest> request = ArgumentCaptor.forClass(ArtifactRequest.class);
    verify(repositorySystem).resolveArtifact(eq(mojo.repositorySession), request.capture());
    assertEquals("com.webforj:webforj-bbjship:zip:bbj-plugin:26.03-SNAPSHOT",
        request.getValue().getArtifact().toString());
    assertEquals("https://repo.example.test/releases",
        request.getValue().getRepositories().get(0).getUrl());
    verify(setupBuilder).setHome(temporary.resolve("bbx"));
    verify(setupBuilder).setDeploymentUrl(URI.create("http://localhost:8888/webforj-bbjship"));
    verify(setupBuilder).setPort(8888);
    verify(setupBuilder).setForce(true);
    ArgumentCaptor<BBjCredentials> credentials = ArgumentCaptor.forClass(BBjCredentials.class);
    verify(setupBuilder).setCredentials(credentials.capture());
    assertEquals("administrator", credentials.getValue().getUsername());
    assertEquals("secret", credentials.getValue().getPassword());
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
    mojo.deploymentUrl = "https://localhost/webforj-bbjship";
    mojo.username = "installer";
    mojo.password = "installer-secret";

    try (var setups = mockStatic(Setup.class)) {
      setups.when(() -> Setup.create(bundle, "26.03-SNAPSHOT")).thenReturn(setupBuilder);

      mojo.execute();
    }

    verify(setupBuilder).setHome(temporary.resolve("bbx-25"));
    verify(setupBuilder).setDeploymentUrl(URI.create("https://localhost/webforj-bbjship"));
    ArgumentCaptor<BBjCredentials> credentials = ArgumentCaptor.forClass(BBjCredentials.class);
    verify(setupBuilder).setCredentials(credentials.capture());
    assertEquals("installer", credentials.getValue().getUsername());
    assertEquals("installer-secret", credentials.getValue().getPassword());
  }

  @Test
  void shouldLeaveTheHomeToSetupWhenNoneIsConfigured() throws Exception {
    mojo.bbj = new BBjOptions();
    mojo.bbj.getDeployment().setUrl("http://localhost:8888/webforj-bbjship");

    try (var setups = mockStatic(Setup.class)) {
      setups.when(() -> Setup.create(bundle, "26.03-SNAPSHOT")).thenReturn(setupBuilder);

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
      setups.when(() -> Setup.create(bundle, "26.03-SNAPSHOT")).thenReturn(setupBuilder);

      mojo.execute();
    }

    verify(setupBuilder).setPort(port);
  }

  @Test
  void shouldRefuseToSetUpWithoutUrl() {
    mojo.bbj.getDeployment().setUrl("");

    MojoExecutionException failure = assertThrows(MojoExecutionException.class, mojo::execute);

    assertEquals("BBj setup requires bbj.deployment.url. Set it in plugin configuration or pass "
        + "-Dwebforj.bbj.deployment.url=<URL>", failure.getMessage());
    verifyNoInteractions(repositorySystem);
  }

  @Test
  void shouldRejectAnInvalidUrlBeforeResolvingTheBbjShipPlugin() {
    mojo.deploymentUrl = "https://localhost/%invalid";

    MojoExecutionException failure = assertThrows(MojoExecutionException.class, mojo::execute);

    assertEquals("BBj setup failed. Check the BBj plugin artifact and setup settings",
        failure.getMessage());
    verifyNoInteractions(repositorySystem);
  }

  @Test
  void shouldReportThatTheBbjShipPluginCannotBeResolved() throws Exception {
    ArtifactResolutionException missing = new ArtifactResolutionException(List.of());
    when(repositorySystem.resolveArtifact(eq(mojo.repositorySession), any())).thenThrow(missing);

    MojoExecutionException failure = assertThrows(MojoExecutionException.class, mojo::execute);

    assertEquals("BBj setup failed. Check the BBj plugin artifact and setup settings",
        failure.getMessage());
    assertSame(missing, failure.getCause());
  }

  @Test
  void shouldSetUpWithThePasswordWhenTheTokenIsConfiguredToo() throws Exception {
    mojo.bbj.getAdmin().setUsername("administrator");
    mojo.bbj.getAdmin().setPassword("secret");
    mojo.bbj.getAdmin().setToken("4f2a9c");

    try (var setups = mockStatic(Setup.class)) {
      setups.when(() -> Setup.create(bundle, "26.03-SNAPSHOT")).thenReturn(setupBuilder);

      mojo.execute();
    }

    ArgumentCaptor<BBjCredentials> credentials = ArgumentCaptor.forClass(BBjCredentials.class);
    verify(setupBuilder).setCredentials(credentials.capture());
    assertFalse(credentials.getValue().isToken());
    assertEquals("administrator", credentials.getValue().getUsername());
    assertEquals("secret", credentials.getValue().getPassword());
  }

  @ParameterizedTest
  @MethodSource("getSetupFailures")
  void shouldReportTheFailureOfTheSetup(Exception setupFailure) throws Exception {
    doThrow(setupFailure).when(setup).apply();

    try (var setups = mockStatic(Setup.class)) {
      setups.when(() -> Setup.create(bundle, "26.03-SNAPSHOT")).thenReturn(setupBuilder);

      MojoExecutionException failure = assertThrows(MojoExecutionException.class, mojo::execute);

      assertEquals("BBj setup failed. Check the BBj plugin artifact and setup settings",
          failure.getMessage());
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
      setups.when(() -> Setup.create(bundle, "26.03-SNAPSHOT")).thenReturn(setupBuilder);

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

    assertEquals(
        "BBj administration requires bbj.admin.username and bbj.admin.password. " + missing,
        failure.getMessage());
  }

  private static Stream<Arguments> getAccounts() {
    return Stream.of(Arguments.of(null, null, 1), Arguments.of("admin", "admin123", 1),
        Arguments.of("admin", "secret", 0));
  }

  private static Stream<Arguments> getIncompleteCredentials() {
    return Stream.of(Arguments.of(null, "secret", "bbj.admin.username is missing"),
        Arguments.of("deployer", null, "bbj.admin.password is missing"));
  }

  private static Stream<Arguments> getPorts() {
    return Stream.of(Arguments.of("https://localhost/webforj-bbjship", 443),
        Arguments.of("http://localhost/webforj-bbjship", 80));
  }

  private static Stream<Arguments> getSetupFailures() {
    return Stream.of(Arguments.of(new IOException("Missing BBj administration credentials")),
        Arguments.of(new IllegalStateException("BBj installation is not writable")));
  }
}
