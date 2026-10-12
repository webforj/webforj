package com.webforj.plugin.maven.bbj;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.plugin.foundation.bbj.model.BBjCredentials;
import com.webforj.plugin.maven.bbj.model.BBjOptions;
import java.io.File;
import java.util.stream.Stream;
import org.apache.maven.plugin.MojoExecutionException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class DeploymentSettingsTest {

  @Test
  void shouldReadConfiguredValuesWhenNoOverrideIsGiven() throws Exception {
    BBjOptions options = new BBjOptions();
    options.setPublishName("hello-app");
    options.getDeployment().setUrl("https://bbj.example.test/webforj-bbjship");
    options.getAdmin().setUsername("administrator");
    options.getAdmin().setPassword("secret");
    options.setHome(new File("/opt/bbx"));
    options.setProperties(new File("/opt/bbx/cfg/BBj.properties"));
    DeploymentSettings settings = DeploymentSettings.create(options);

    BBjCredentials credentials = settings.getCredentials();

    assertEquals("https://bbj.example.test/webforj-bbjship", settings.getUrl());
    assertEquals("hello-app", settings.getPublishName());
    assertEquals("administrator", credentials.getUsername());
    assertEquals("secret", credentials.getPassword());
    assertEquals(new File("/opt/bbx"), settings.getHome());
    assertEquals(new File("/opt/bbx/cfg/BBj.properties"), settings.getProperties());
  }

  @Test
  void shouldPreferOverridesAndCarryTheVersion() throws Exception {
    BBjOptions options = new BBjOptions();
    options.setPublishName("hello-app");
    options.getDeployment().setUrl("https://bbj.example.test/webforj-bbjship");
    options.getAdmin().setUsername("administrator");
    options.getAdmin().setPassword("secret");
    options.setHome(new File("/opt/bbx"));
    options.setProperties(new File("/opt/bbx/cfg/BBj.properties"));
    DeploymentSettings settings = DeploymentSettings.create(options)
        .setUrl("http://localhost:8888/webforj-bbjship").setPublishName("hello-app-staging")
        .setUsername("deployer").setPassword("deployer-secret").setVersion("26.03")
        .setHome(new File("/opt/bbx-25")).setProperties(new File("/etc/bbj/BBj.properties"));

    BBjCredentials credentials = settings.getCredentials();

    assertEquals("http://localhost:8888/webforj-bbjship", settings.getUrl());
    assertEquals("hello-app-staging", settings.getPublishName());
    assertEquals("deployer", credentials.getUsername());
    assertEquals("deployer-secret", credentials.getPassword());
    assertEquals("26.03", settings.getVersion());
    assertEquals(new File("/opt/bbx-25"), settings.getHome());
    assertEquals(new File("/etc/bbj/BBj.properties"), settings.getProperties());
  }

  @ParameterizedTest
  @MethodSource("getPublishNames")
  void shouldFallBackToTheDefaultPublishNameOnlyWhenNothingIsConfigured(String defaultName,
      String override, String expected) {
    BBjOptions options = new BBjOptions();
    DeploymentSettings settings = DeploymentSettings.create(options)
        .setDefaultPublishName(defaultName).setPublishName(override);

    assertEquals(expected, settings.getPublishName());
  }

  @Test
  void shouldTreatAnEmptyOverrideAsAnOverride() {
    BBjOptions options = new BBjOptions();
    options.getDeployment().setUrl("https://bbj.example.test/webforj-bbjship");
    DeploymentSettings settings = DeploymentSettings.create(options).setUrl("");

    assertEquals("", settings.getUrl());
  }

  @Test
  void shouldDefaultTheAdministrationCredentialsWhenNothingIsConfigured() throws Exception {
    BBjOptions options = new BBjOptions();

    BBjCredentials credentials = DeploymentSettings.create(options).getCredentials();

    assertEquals("admin", credentials.getUsername());
    assertEquals("admin123", credentials.getPassword());
  }

  @ParameterizedTest
  @MethodSource("getIncompleteCredentials")
  void shouldRefuseIncompleteCredentials(String configuredUsername, String configuredPassword,
      String username, String password, String missing) {
    BBjOptions options = new BBjOptions();
    if (configuredUsername != null) {
      options.getAdmin().setUsername(configuredUsername);
    }

    if (configuredPassword != null) {
      options.getAdmin().setPassword(configuredPassword);
    }

    DeploymentSettings settings =
        DeploymentSettings.create(options).setUsername(username).setPassword(password);

    MojoExecutionException withToken =
        assertThrows(MojoExecutionException.class, settings::getCredentials);
    MojoExecutionException withoutToken =
        assertThrows(MojoExecutionException.class, settings::getPasswordCredentials);

    assertEquals("BBj administration requires bbj.admin.username and bbj.admin.password, or "
        + "webforj.bbj.admin.token. " + missing, withToken.getMessage());
    assertEquals(
        "BBj administration requires bbj.admin.username and bbj.admin.password. " + missing,
        withoutToken.getMessage());
  }

  @ParameterizedTest
  @MethodSource("getTokens")
  void shouldPreferNonBlankTokenOverUsernameAndPassword(String configured, String override,
      String expected) throws Exception {
    BBjOptions options = new BBjOptions();
    options.getAdmin().setUsername("administrator");
    options.getAdmin().setPassword("secret");
    options.getAdmin().setToken(configured);
    DeploymentSettings settings = DeploymentSettings.create(options).setToken(override);

    BBjCredentials credentials = settings.getCredentials();

    assertTrue(credentials.isToken());
    assertEquals(expected, credentials.getToken());
  }

  @Test
  void shouldGiveUsernameAndPasswordWhenTheTokenIsIgnored() throws Exception {
    BBjOptions options = new BBjOptions();
    options.getAdmin().setUsername("administrator");
    options.getAdmin().setPassword("secret");
    options.getAdmin().setToken("4f2a9c");
    DeploymentSettings settings = DeploymentSettings.create(options);

    BBjCredentials credentials = settings.getPasswordCredentials();

    assertFalse(credentials.isToken());
    assertEquals("administrator", credentials.getUsername());
    assertEquals("secret", credentials.getPassword());
  }

  @ParameterizedTest
  @MethodSource("getBlankTokens")
  void shouldUseUsernameAndPasswordWhenTheTokenIsBlank(String configured, String override)
      throws Exception {
    BBjOptions options = new BBjOptions();
    options.getAdmin().setUsername("administrator");
    options.getAdmin().setPassword("secret");
    options.getAdmin().setToken(configured);
    DeploymentSettings settings = DeploymentSettings.create(options).setToken(override);

    BBjCredentials credentials = settings.getCredentials();

    assertFalse(credentials.isToken());
    assertEquals("administrator", credentials.getUsername());
    assertEquals("secret", credentials.getPassword());
  }

  @Test
  void shouldRequireOptions() {
    BBjOptions missing = null;

    assertThrows(NullPointerException.class, () -> DeploymentSettings.create(missing));
  }

  @ParameterizedTest
  @MethodSource("getPorts")
  void shouldGiveThePortOfTheUrl(String url, int port) {
    BBjOptions options = new BBjOptions();
    options.getDeployment().setUrl(url);

    assertEquals(port, DeploymentSettings.create(options).getPort());
  }

  private static Stream<Arguments> getIncompleteCredentials() {
    return Stream.of(Arguments.of(null, "secret", null, null, "bbj.admin.username is missing"),
        Arguments.of("administrator", null, null, null, "bbj.admin.password is missing"),
        Arguments.of(null, null, null, "secret", "bbj.admin.username is missing"),
        Arguments.of("", "secret", null, null, "bbj.admin.username is empty"),
        Arguments.of("administrator", "", null, null, "bbj.admin.password is empty"),
        Arguments.of("administrator", "secret", " ", null, "bbj.admin.username is empty"),
        Arguments.of("administrator", "secret", null, "", "bbj.admin.password is empty"));
  }

  private static Stream<Arguments> getTokens() {
    return Stream.of(Arguments.of("4f2a9c", null, "4f2a9c"),
        Arguments.of("4f2a9c", "7d81e0", "7d81e0"), Arguments.of("", "7d81e0", "7d81e0"));
  }

  private static Stream<Arguments> getBlankTokens() {
    return Stream.of(Arguments.of("", null), Arguments.of("  ", null), Arguments.of("4f2a9c", ""));
  }

  private static Stream<Arguments> getPublishNames() {
    return Stream.of(Arguments.of("hello-app-1.0-SNAPSHOT", null, "hello-app-1.0-SNAPSHOT"),
        Arguments.of(null, null, ""),
        Arguments.of("hello-app-1.0-SNAPSHOT", "hello-app", "hello-app"));
  }

  private static Stream<Arguments> getPorts() {
    return Stream.of(Arguments.of("http://localhost:8888/webforj-bbjship", 8888),
        Arguments.of("https://localhost/webforj-bbjship", 443),
        Arguments.of("http://localhost/webforj-bbjship", 80));
  }
}
