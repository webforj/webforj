package com.webforj.plugin.foundation.bbj;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.plugin.foundation.bbj.DeploymentPolicy.Authentication;
import com.webforj.plugin.foundation.bbj.model.BBjCredentials;
import java.net.URI;
import java.nio.file.Path;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.MethodSource;

class DeploymentPolicyTest {

  @ParameterizedTest
  @EnumSource(Authentication.class)
  void shouldGiveTheUsernameAndPassword(Authentication authentication) {
    BBjCredentials credentials =
        DeploymentPolicy.resolveCredentials("administrator", "secret", null, authentication);

    assertFalse(credentials.isToken());
    assertEquals("administrator", credentials.getUsername());
    assertEquals("secret", credentials.getPassword());
  }

  @ParameterizedTest
  @EnumSource(Authentication.class)
  void shouldGiveTheFactoryAccountWhenNothingIsConfigured(Authentication authentication) {
    BBjCredentials credentials =
        DeploymentPolicy.resolveCredentials(null, null, null, authentication);

    assertTrue(credentials.isFactoryAccount());
    assertEquals("admin", credentials.getUsername());
    assertEquals("admin123", credentials.getPassword());
  }

  @Test
  void shouldPreferTheTokenOverTheUsernameAndPassword() {
    BBjCredentials credentials = DeploymentPolicy.resolveCredentials("administrator", "secret",
        "4f2a9c", Authentication.PASSWORD_OR_TOKEN);

    assertTrue(credentials.isToken());
    assertEquals("4f2a9c", credentials.getToken());
  }

  @ParameterizedTest
  @MethodSource("getIgnoredTokens")
  void shouldGiveTheUsernameAndPasswordWhenTheTokenIsNotUsed(String token,
      Authentication authentication) {
    BBjCredentials credentials =
        DeploymentPolicy.resolveCredentials("administrator", "secret", token, authentication);

    assertFalse(credentials.isToken());
    assertEquals("administrator", credentials.getUsername());
  }

  @ParameterizedTest
  @MethodSource("getIncompleteCredentials")
  void shouldRefuseIncompleteCredentials(String username, String password,
      Authentication authentication, String message) {
    IllegalArgumentException failure = assertThrows(IllegalArgumentException.class,
        () -> DeploymentPolicy.resolveCredentials(username, password, null, authentication));

    assertEquals(message, failure.getMessage());
  }

  @ParameterizedTest
  @MethodSource("getPorts")
  void shouldGiveThePortOfTheUrl(String url, int port) {
    assertEquals(port, DeploymentPolicy.getPort(URI.create(url)));
  }

  @Test
  void shouldNameTheArchiveAfterThePublishNameInTheBuildDirectory() {
    Path archive = DeploymentPolicy.resolveArchive(Path.of("/work/hello-app/build"), "hello-app");

    assertEquals(Path.of("/work/hello-app/build/bbj/hello-app.zip"), archive);
  }

  @ParameterizedTest
  @MethodSource("getRefusedPublishNames")
  void shouldRefuseThePublishNameOfTheArchive(String publishName, String message) {
    Path buildDirectory = Path.of("/work/hello-app/build");

    IllegalArgumentException failure = assertThrows(IllegalArgumentException.class,
        () -> DeploymentPolicy.resolveArchive(buildDirectory, publishName));

    assertEquals(message, failure.getMessage());
  }

  private static Stream<Arguments> getIgnoredTokens() {
    return Stream.of(Arguments.of("4f2a9c", Authentication.PASSWORD),
        Arguments.of("", Authentication.PASSWORD_OR_TOKEN),
        Arguments.of("  ", Authentication.PASSWORD_OR_TOKEN));
  }

  private static Stream<Arguments> getIncompleteCredentials() {
    return Stream.of(
        Arguments.of(null, "secret", Authentication.PASSWORD,
            "BBj administration requires bbj.admin.username and bbj.admin.password. "
                + "bbj.admin.username is missing"),
        Arguments.of("administrator", null, Authentication.PASSWORD,
            "BBj administration requires bbj.admin.username and bbj.admin.password. "
                + "bbj.admin.password is missing"),
        Arguments.of(" ", "secret", Authentication.PASSWORD_OR_TOKEN,
            "BBj administration requires bbj.admin.username and bbj.admin.password, or "
                + "webforj.bbj.admin.token. bbj.admin.username is empty"),
        Arguments.of("administrator", "", Authentication.PASSWORD_OR_TOKEN,
            "BBj administration requires bbj.admin.username and bbj.admin.password, or "
                + "webforj.bbj.admin.token. bbj.admin.password is empty"));
  }

  private static Stream<Arguments> getPorts() {
    return Stream.of(Arguments.of("http://localhost:8888/webforj-bbjship", 8888),
        Arguments.of("https://localhost/webforj-bbjship", 443),
        Arguments.of("http://localhost/webforj-bbjship", 80));
  }

  private static Stream<Arguments> getRefusedPublishNames() {
    return Stream.of(
        Arguments.of("", "BBj deployment requires bbj.publishName or a final name of the build"),
        Arguments.of("../escaped",
            "Publish name ../escaped is invalid. Use a name that starts with a letter or digit, "
                + "holds only letters, digits, underscores, dots and hyphens, and is not a "
                + "reserved device name such as con or nul"),
        Arguments.of("webapp", "The publish name webapp is reserved by BBjServices for its own "
            + "web paths. Choose another publish name"));
  }
}
