package com.webforj.plugin.maven;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.plugin.foundation.bbj.BBjShipClient;
import com.webforj.plugin.foundation.bbj.model.BBjCredentials;
import com.webforj.plugin.maven.bbj.model.BBjAdminOptions;
import java.io.IOException;
import java.net.URI;
import java.util.function.Consumer;
import java.util.stream.Stream;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.descriptor.PluginDescriptor;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.Answers;
import org.mockito.ArgumentCaptor;

class BBjUndeployMojoTest {

  Log log;
  BBjUndeployMojo mojo;
  BBjShipClient bbjShip;
  BBjShipClient.Builder bbjShipBuilder;

  @BeforeEach
  void createMojo() {
    mojo = new BBjUndeployMojo();
    mojo.project = new MavenProject();
    mojo.pluginDescriptor = new PluginDescriptor();
    mojo.pluginDescriptor.setVersion("26.03-SNAPSHOT");
    mojo.bbj.setPublishName("hello-app");
    mojo.bbj.getDeployment().setUrl("https://bbj.example.test/webforj-bbjship");
    mojo.bbj.getAdmin().setUsername("administrator");
    mojo.bbj.getAdmin().setPassword("secret");
    log = mock(Log.class);
    mojo.setLog(log);
    bbjShip = mock(BBjShipClient.class);
    bbjShipBuilder = mock(BBjShipClient.Builder.class, Answers.RETURNS_SELF);
    when(bbjShipBuilder.build()).thenReturn(bbjShip);
  }

  @Test
  void shouldUndeployThePublishNameWithTheConfiguredSettings() throws Exception {
    try (var bbjShips = mockStatic(BBjShipClient.class)) {
      bbjShips
          .when(() -> BBjShipClient.create(URI.create("https://bbj.example.test/webforj-bbjship")))
          .thenReturn(bbjShipBuilder);

      mojo.execute();
    }

    ArgumentCaptor<BBjCredentials> credentials = ArgumentCaptor.forClass(BBjCredentials.class);
    verify(bbjShipBuilder).setCredentials(credentials.capture());
    assertEquals("administrator", credentials.getValue().getUsername());
    assertEquals("secret", credentials.getValue().getPassword());
    verify(bbjShipBuilder).setVersion("26.03-SNAPSHOT");
    ArgumentCaptor<Consumer<String>> logChannel = ArgumentCaptor.captor();
    verify(bbjShipBuilder).setLog(logChannel.capture());
    logChannel.getValue().accept("Removing hello-app");
    verify(log).info("Removing hello-app");
    ArgumentCaptor<Consumer<String>> warnChannel = ArgumentCaptor.captor();
    verify(bbjShipBuilder).setWarn(warnChannel.capture());
    warnChannel.getValue().accept("Credentials are readable over plain HTTP");
    verify(log).warn("Credentials are readable over plain HTTP");
    verify(bbjShip).undeploy("hello-app");
    verify(log).info("Undeploying hello-app from https://bbj.example.test/webforj-bbjship");
    verify(log).info("BBj undeployment of hello-app completed");
  }

  @Test
  void shouldUndeployTheBuildFinalNameWhenNoPublishNameIsConfigured() throws Exception {
    mojo.bbj.setPublishName("");
    mojo.project.getBuild().setFinalName("hello-app-1.0-SNAPSHOT");

    try (var bbjShips = mockStatic(BBjShipClient.class)) {
      bbjShips
          .when(() -> BBjShipClient.create(URI.create("https://bbj.example.test/webforj-bbjship")))
          .thenReturn(bbjShipBuilder);

      mojo.execute();
    }

    verify(bbjShip).undeploy("hello-app-1.0-SNAPSHOT");
  }

  @Test
  void shouldPassTheTokenInPlaceOfUsernameAndPassword() throws Exception {
    mojo.token = "4f2a9c";

    try (var bbjShips = mockStatic(BBjShipClient.class)) {
      bbjShips
          .when(() -> BBjShipClient.create(URI.create("https://bbj.example.test/webforj-bbjship")))
          .thenReturn(bbjShipBuilder);

      mojo.execute();
    }

    ArgumentCaptor<BBjCredentials> credentials = ArgumentCaptor.forClass(BBjCredentials.class);
    verify(bbjShipBuilder).setCredentials(credentials.capture());
    assertTrue(credentials.getValue().isToken());
    assertEquals("4f2a9c", credentials.getValue().getToken());
    verify(bbjShip).undeploy("hello-app");
  }

  @Test
  void shouldPreferTheCommandLineOverTheConfiguration() throws Exception {
    mojo.deploymentUrl = "http://localhost:8888/webforj-bbjship";
    mojo.publishName = "hello-app-staging";
    mojo.username = "deployer";
    mojo.password = "deployer-secret";

    try (var bbjShips = mockStatic(BBjShipClient.class)) {
      bbjShips.when(() -> BBjShipClient.create(URI.create("http://localhost:8888/webforj-bbjship")))
          .thenReturn(bbjShipBuilder);

      mojo.execute();
    }

    ArgumentCaptor<BBjCredentials> credentials = ArgumentCaptor.forClass(BBjCredentials.class);
    verify(bbjShipBuilder).setCredentials(credentials.capture());
    assertEquals("deployer", credentials.getValue().getUsername());
    assertEquals("deployer-secret", credentials.getValue().getPassword());
    verify(bbjShip).undeploy("hello-app-staging");
  }

  @Test
  void shouldReportTheFailureOfTheBbjShip() throws Exception {
    IOException requestFailure = new IOException("BBj undeployment request failed");
    doThrow(requestFailure).when(bbjShip).undeploy("hello-app");

    try (var bbjShips = mockStatic(BBjShipClient.class)) {
      bbjShips
          .when(() -> BBjShipClient.create(URI.create("https://bbj.example.test/webforj-bbjship")))
          .thenReturn(bbjShipBuilder);

      MojoExecutionException failure = assertThrows(MojoExecutionException.class, mojo::execute);

      assertEquals("BBj undeployment failed. Check the endpoint, administration credentials and "
          + "BBjServices log", failure.getMessage());
      assertSame(requestFailure, failure.getCause());
    }
  }

  @ParameterizedTest
  @MethodSource("getIncompleteSettings")
  void shouldRefuseToUndeployWithoutUrlOrPublishName(String url, String publishName,
      String message) {
    mojo.bbj.getDeployment().setUrl(url);
    mojo.bbj.setPublishName(publishName);

    try (var bbjShips = mockStatic(BBjShipClient.class)) {
      MojoExecutionException failure = assertThrows(MojoExecutionException.class, mojo::execute);

      assertEquals(message, failure.getMessage());
      bbjShips.verifyNoInteractions();
    }
  }

  @ParameterizedTest
  @MethodSource("getAccounts")
  void shouldWarnWhileTheFactoryAccountIsInUse(String username, String password, int warnings)
      throws Exception {
    mojo.bbj.setAdmin(new BBjAdminOptions());
    mojo.username = username;
    mojo.password = password;

    try (var bbjShips = mockStatic(BBjShipClient.class)) {
      bbjShips
          .when(() -> BBjShipClient.create(URI.create("https://bbj.example.test/webforj-bbjship")))
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

    assertEquals("BBj administration requires bbj.admin.username and bbj.admin.password, or "
        + "webforj.bbj.admin.token. " + missing, failure.getMessage());
  }

  private static Stream<Arguments> getAccounts() {
    return Stream.of(Arguments.of(null, null, 1), Arguments.of("admin", "admin123", 1),
        Arguments.of("admin", "secret", 0));
  }

  private static Stream<Arguments> getIncompleteCredentials() {
    return Stream.of(Arguments.of(null, "secret", "bbj.admin.username is missing"),
        Arguments.of("deployer", null, "bbj.admin.password is missing"));
  }

  private static Stream<Arguments> getIncompleteSettings() {
    return Stream.of(
        Arguments.of("", "hello-app",
            "BBj undeployment requires bbj.deployment.url. Set it in plugin configuration or "
                + "pass -Dwebforj.bbj.deployment.url=<URL>"),
        Arguments.of("https://bbj.example.test/webforj-bbjship", "",
            "BBj undeployment requires bbj.publishName or a final name of the build"));
  }
}
