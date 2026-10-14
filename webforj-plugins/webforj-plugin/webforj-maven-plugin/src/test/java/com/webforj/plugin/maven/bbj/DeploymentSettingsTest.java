package com.webforj.plugin.maven.bbj;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.bbjship.foundation.model.BBjCredentials;
import com.webforj.plugin.maven.bbj.model.BBjOptions;
import java.io.File;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;
import org.apache.maven.plugin.MojoExecutionException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;

class DeploymentSettingsTest {

  private static final String PUBLISH_NAME = "hello-app";
  private static final String DEFAULT_PUBLISH_NAME = "hello-app-1.0-SNAPSHOT";
  private static final String DEPLOYMENT_URL = "https://bbj.example.test/webforj-bbjship";
  private static final String LOCAL_DEPLOYMENT_URL = "http://localhost:8888/webforj-bbjship";
  private static final String ADMIN_USERNAME = "administrator";
  private static final String ADMIN_PASSWORD = "secret";
  private static final String CONFIGURED_TOKEN = "4f2a9c";
  private static final String OVERRIDE_TOKEN = "7d81e0";
  private static final String BBJ_HOME = "/opt/bbx";
  private static final String PROPERTIES_PATH = "/opt/bbx/cfg/BBj.properties"; // NOSONAR
  private static final String OVERRIDE_HOME = "/opt/bbx-25";
  private static final String OVERRIDE_PROPERTIES_PATH = "/etc/bbj/BBj.properties"; // NOSONAR
  private static final String MISSING_USER_NAME_MESSAGE =
      "The BBj administration user name is missing";
  private static final String MISSING_PASSWORD_MESSAGE =
      "The BBj administration password is missing";

  List<String> warnings = new ArrayList<>();

  @Test
  void shouldReadConfiguredValuesWhenNoOverrideIsGiven() throws Exception {
    BBjOptions options = new BBjOptions();
    options.setPublishName(PUBLISH_NAME);
    options.getDeployment().setUrl(DEPLOYMENT_URL);
    options.getAdmin().setUsername(ADMIN_USERNAME);
    options.getAdmin().setPassword(ADMIN_PASSWORD);
    options.setHome(new File(BBJ_HOME));
    options.setProperties(new File(PROPERTIES_PATH));
    DeploymentSettings settings = DeploymentSettings.create(options);

    BBjCredentials credentials = settings.getCredentials(warnings::add);

    assertEquals(DEPLOYMENT_URL, settings.getUrl());
    assertEquals(PUBLISH_NAME, settings.getPublishName());
    assertEquals(ADMIN_USERNAME, credentials.getUsername());
    assertEquals(ADMIN_PASSWORD, credentials.getPassword());
    assertEquals(new File(BBJ_HOME), settings.getHome());
    assertEquals(new File(PROPERTIES_PATH), settings.getProperties());
  }

  @Test
  void shouldPreferOverridesAndCarryTheVersion() throws Exception {
    BBjOptions options = new BBjOptions();
    options.setPublishName(PUBLISH_NAME);
    options.getDeployment().setUrl(DEPLOYMENT_URL);
    options.getAdmin().setUsername(ADMIN_USERNAME);
    options.getAdmin().setPassword(ADMIN_PASSWORD);
    options.setHome(new File(BBJ_HOME));
    options.setProperties(new File(PROPERTIES_PATH));
    DeploymentSettings settings = DeploymentSettings.create(options).setUrl(LOCAL_DEPLOYMENT_URL)
        .setPublishName("hello-app-staging").setUsername("deployer").setPassword("deployer-secret")
        .setVersion("26.03").setHome(new File(OVERRIDE_HOME))
        .setProperties(new File(OVERRIDE_PROPERTIES_PATH));

    BBjCredentials credentials = settings.getCredentials(warnings::add);

    assertEquals(LOCAL_DEPLOYMENT_URL, settings.getUrl());
    assertEquals("hello-app-staging", settings.getPublishName());
    assertEquals("deployer", credentials.getUsername());
    assertEquals("deployer-secret", credentials.getPassword());
    assertEquals("26.03", settings.getVersion());
    assertEquals(new File(OVERRIDE_HOME), settings.getHome());
    assertEquals(new File(OVERRIDE_PROPERTIES_PATH), settings.getProperties());
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
    options.getDeployment().setUrl(DEPLOYMENT_URL);
    DeploymentSettings settings = DeploymentSettings.create(options).setUrl("");

    assertEquals("", settings.getUrl());
  }

  @Test
  void shouldDefaultTheAdministrationCredentialsWhenNothingIsConfigured() throws Exception {
    BBjOptions options = new BBjOptions();

    BBjCredentials credentials = DeploymentSettings.create(options).getCredentials(warnings::add);

    assertEquals("admin", credentials.getUsername());
    assertEquals("admin123", credentials.getPassword());
    assertEquals(List.of(BBjCredentials.FACTORY_ACCOUNT_WARNING), warnings);
  }

  @ParameterizedTest
  @ValueSource(strings = {"", " "})
  void shouldRefuseToResolveBlankUrl(String url) {
    DeploymentSettings settings = DeploymentSettings.create(new BBjOptions()).setUrl(url);

    MojoExecutionException failure =
        assertThrows(MojoExecutionException.class, settings::resolveUrl);

    assertEquals("BBj deployment requires bbj.deployment.url. Set it in plugin configuration or "
        + "pass -Dwebforj.bbj.deployment.url=<URL>", failure.getMessage());
  }

  @Test
  void shouldResolveTheEffectiveUrl() throws Exception {
    DeploymentSettings settings =
        DeploymentSettings.create(new BBjOptions()).setUrl(LOCAL_DEPLOYMENT_URL);

    assertEquals(URI.create(LOCAL_DEPLOYMENT_URL), settings.resolveUrl());
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
        assertThrows(MojoExecutionException.class, () -> settings.getCredentials(warnings::add));
    MojoExecutionException withoutToken = assertThrows(MojoExecutionException.class,
        () -> settings.getPasswordCredentials(warnings::add));

    assertEquals(missing + ". Configure bbj.admin.username and bbj.admin.password, or "
        + "webforj.bbj.admin.token", withToken.getMessage());
    assertEquals(missing + ". Configure bbj.admin.username and bbj.admin.password",
        withoutToken.getMessage());
  }

  @ParameterizedTest
  @MethodSource("getTokens")
  void shouldPreferNonBlankTokenOverUsernameAndPassword(String configured, String override,
      String expected) throws Exception {
    BBjOptions options = new BBjOptions();
    options.getAdmin().setUsername(ADMIN_USERNAME);
    options.getAdmin().setPassword(ADMIN_PASSWORD);
    options.getAdmin().setToken(configured);
    DeploymentSettings settings = DeploymentSettings.create(options).setToken(override);

    BBjCredentials credentials = settings.getCredentials(warnings::add);

    assertTrue(credentials.isToken());
    assertEquals(expected, credentials.getToken());
  }

  @Test
  void shouldGiveUsernameAndPasswordWhenTheTokenIsIgnored() throws Exception {
    BBjOptions options = new BBjOptions();
    options.getAdmin().setUsername(ADMIN_USERNAME);
    options.getAdmin().setPassword(ADMIN_PASSWORD);
    options.getAdmin().setToken(CONFIGURED_TOKEN);
    DeploymentSettings settings = DeploymentSettings.create(options);

    BBjCredentials credentials = settings.getPasswordCredentials(warnings::add);

    assertFalse(credentials.isToken());
    assertEquals(ADMIN_USERNAME, credentials.getUsername());
    assertEquals(ADMIN_PASSWORD, credentials.getPassword());
  }

  @ParameterizedTest
  @MethodSource("getBlankTokens")
  void shouldUseUsernameAndPasswordWhenTheTokenIsBlank(String configured, String override)
      throws Exception {
    BBjOptions options = new BBjOptions();
    options.getAdmin().setUsername(ADMIN_USERNAME);
    options.getAdmin().setPassword(ADMIN_PASSWORD);
    options.getAdmin().setToken(configured);
    DeploymentSettings settings = DeploymentSettings.create(options).setToken(override);

    BBjCredentials credentials = settings.getCredentials(warnings::add);

    assertFalse(credentials.isToken());
    assertEquals(ADMIN_USERNAME, credentials.getUsername());
    assertEquals(ADMIN_PASSWORD, credentials.getPassword());
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
    return Stream.of(Arguments.of(null, ADMIN_PASSWORD, null, null, MISSING_USER_NAME_MESSAGE),
        Arguments.of(ADMIN_USERNAME, null, null, null, MISSING_PASSWORD_MESSAGE),
        Arguments.of(null, null, null, ADMIN_PASSWORD, MISSING_USER_NAME_MESSAGE),
        Arguments.of("", ADMIN_PASSWORD, null, null, MISSING_USER_NAME_MESSAGE),
        Arguments.of(ADMIN_USERNAME, "", null, null, MISSING_PASSWORD_MESSAGE),
        Arguments.of(ADMIN_USERNAME, ADMIN_PASSWORD, " ", null, MISSING_USER_NAME_MESSAGE),
        Arguments.of(ADMIN_USERNAME, ADMIN_PASSWORD, null, "", MISSING_PASSWORD_MESSAGE), Arguments
            .of("", "", null, null, "The BBj administration user name and password are missing"));
  }

  private static Stream<Arguments> getTokens() {
    return Stream.of(Arguments.of(CONFIGURED_TOKEN, null, CONFIGURED_TOKEN),
        Arguments.of(CONFIGURED_TOKEN, OVERRIDE_TOKEN, OVERRIDE_TOKEN),
        Arguments.of("", OVERRIDE_TOKEN, OVERRIDE_TOKEN));
  }

  private static Stream<Arguments> getBlankTokens() {
    return Stream.of(Arguments.of("", null), Arguments.of("  ", null),
        Arguments.of(CONFIGURED_TOKEN, ""));
  }

  private static Stream<Arguments> getPublishNames() {
    return Stream.of(Arguments.of(DEFAULT_PUBLISH_NAME, null, DEFAULT_PUBLISH_NAME),
        Arguments.of(null, null, ""),
        Arguments.of(DEFAULT_PUBLISH_NAME, PUBLISH_NAME, PUBLISH_NAME));
  }

  private static Stream<Arguments> getPorts() {
    return Stream.of(Arguments.of(LOCAL_DEPLOYMENT_URL, 8888),
        Arguments.of("https://localhost/webforj-bbjship", 443),
        Arguments.of("http://localhost/webforj-bbjship", 80));
  }
}
