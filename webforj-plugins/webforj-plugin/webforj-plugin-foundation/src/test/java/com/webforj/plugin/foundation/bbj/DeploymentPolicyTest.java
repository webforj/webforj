package com.webforj.plugin.foundation.bbj;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.plugin.foundation.bbj.DeploymentPolicy.Authentication;
import com.webforj.plugin.foundation.bbj.model.BBjCredentials;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.MethodSource;

class DeploymentPolicyTest {

  private static final String ADMIN_USERNAME = "administrator";
  private static final String ADMIN_PASSWORD = "secret";
  private static final String ACCESS_TOKEN = "4f2a9c";

  List<String> warnings = new ArrayList<>();

  @ParameterizedTest
  @EnumSource(Authentication.class)
  void shouldGiveTheUsernameAndPassword(Authentication authentication) {
    BBjCredentials credentials = DeploymentPolicy.resolveCredentials(ADMIN_USERNAME, ADMIN_PASSWORD,
        null, authentication, warnings::add);

    assertFalse(credentials.isToken());
    assertEquals(ADMIN_USERNAME, credentials.getUsername());
    assertEquals(ADMIN_PASSWORD, credentials.getPassword());
    assertEquals(List.of(), warnings);
  }

  @ParameterizedTest
  @EnumSource(Authentication.class)
  void shouldGiveTheFactoryAccountWhenNothingIsConfigured(Authentication authentication) {
    BBjCredentials credentials =
        DeploymentPolicy.resolveCredentials(null, null, null, authentication, warnings::add);

    assertTrue(credentials.isFactoryAccount());
    assertEquals("admin", credentials.getUsername());
    assertEquals("admin123", credentials.getPassword());
    assertEquals(List.of(BBjCredentials.FACTORY_ACCOUNT_WARNING), warnings);
  }

  @Test
  void shouldWarnWhenTheFactoryAccountIsConfigured() {
    DeploymentPolicy.resolveCredentials("admin", "admin123", null, Authentication.PASSWORD,
        warnings::add);

    assertEquals(List.of(BBjCredentials.FACTORY_ACCOUNT_WARNING), warnings);
  }

  @Test
  void shouldPreferTheTokenOverTheUsernameAndPassword() {
    BBjCredentials credentials = DeploymentPolicy.resolveCredentials(ADMIN_USERNAME, ADMIN_PASSWORD,
        ACCESS_TOKEN, Authentication.PASSWORD_OR_TOKEN, warnings::add);

    assertTrue(credentials.isToken());
    assertEquals(ACCESS_TOKEN, credentials.getToken());
  }

  @ParameterizedTest
  @MethodSource("getIgnoredTokens")
  void shouldGiveTheUsernameAndPasswordWhenTheTokenIsNotUsed(String token,
      Authentication authentication) {
    BBjCredentials credentials = DeploymentPolicy.resolveCredentials(ADMIN_USERNAME, ADMIN_PASSWORD,
        token, authentication, warnings::add);

    assertFalse(credentials.isToken());
    assertEquals(ADMIN_USERNAME, credentials.getUsername());
  }

  @ParameterizedTest
  @MethodSource("getIncompleteCredentials")
  void shouldRefuseIncompleteCredentials(String username, String password,
      Authentication authentication, String message) {
    IllegalArgumentException failure =
        assertThrows(IllegalArgumentException.class, () -> DeploymentPolicy
            .resolveCredentials(username, password, null, authentication, warnings::add));

    assertEquals(message, failure.getMessage());
  }

  private static Stream<Arguments> getIgnoredTokens() {
    return Stream.of(Arguments.of(ACCESS_TOKEN, Authentication.PASSWORD),
        Arguments.of("", Authentication.PASSWORD_OR_TOKEN),
        Arguments.of("  ", Authentication.PASSWORD_OR_TOKEN));
  }

  private static Stream<Arguments> getIncompleteCredentials() {
    return Stream.of(
        Arguments.of(null, ADMIN_PASSWORD, Authentication.PASSWORD,
            "The BBj administration user name is missing"),
        Arguments.of(ADMIN_USERNAME, null, Authentication.PASSWORD,
            "The BBj administration password is missing"),
        Arguments.of(" ", ADMIN_PASSWORD, Authentication.PASSWORD_OR_TOKEN,
            "The BBj administration user name is missing"),
        Arguments.of(ADMIN_USERNAME, "", Authentication.PASSWORD_OR_TOKEN,
            "The BBj administration password is missing"),
        Arguments.of("", " ", Authentication.PASSWORD,
            "The BBj administration user name and password are missing"),
        Arguments.of(null, "", Authentication.PASSWORD_OR_TOKEN,
            "The BBj administration user name and password are missing"));
  }
}
