package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.typesafe.config.ConfigFactory;
import com.webforj.router.history.Location;
import java.util.Optional;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class BBjRouteSecurityConfigurationTest {

  private BBjSecurityConfigurationProperties properties;
  private BBjRouteSecurityConfiguration configuration;

  @BeforeEach
  void setUp() {
    properties = new BBjSecurityConfigurationProperties(ConfigFactory.empty());
    configuration = new BBjRouteSecurityConfiguration(properties);
  }

  @Test
  void shouldRejectNullProperties() {
    BBjSecurityConfigurationProperties missing = null;

    assertThrows(NullPointerException.class, () -> new BBjRouteSecurityConfiguration(missing));
  }

  @Nested
  class EnabledStatus {

    @ParameterizedTest
    @MethodSource("enabledStatusCases")
    void shouldHandleEnabledStatus(Boolean enabled, boolean expected) {
      properties.setEnabled(enabled);

      assertEquals(expected, configuration.isEnabled());
    }

    private static Stream<Arguments> enabledStatusCases() {
      return Stream.of(Arguments.of(Boolean.TRUE, true), Arguments.of(Boolean.FALSE, false),
          Arguments.of(null, false));
    }
  }

  @Nested
  class SecureByDefaultStatus {

    @ParameterizedTest
    @MethodSource("secureByDefaultCases")
    void shouldHandleSecureByDefaultStatus(Boolean secureByDefault, boolean expected) {
      properties.setSecureByDefault(secureByDefault);

      assertEquals(expected, configuration.isSecureByDefault());
    }

    private static Stream<Arguments> secureByDefaultCases() {
      return Stream.of(Arguments.of(Boolean.TRUE, true), Arguments.of(Boolean.FALSE, false),
          Arguments.of(null, false));
    }
  }

  @Nested
  class AuthenticationLocation {

    @ParameterizedTest
    @MethodSource("authenticationLocationCases")
    void shouldHandleAuthenticationPaths(String configuredPath, String expectedPath) {
      properties.setAuthenticationPath(configuredPath);

      Optional<Location> location = configuration.getAuthenticationLocation();

      assertTrue(location.isPresent());
      assertEquals(expectedPath, location.get().getFullURI());
    }

    private static Stream<Arguments> authenticationLocationCases() {
      return Stream.of(Arguments.of("/signin", "/signin"), Arguments.of(null, "/login"),
          Arguments.of("/auth?redirect=true", "/auth?redirect=true"),
          Arguments.of("/auth#login-form", "/auth#login-form"));
    }
  }

  @Nested
  class DenyLocation {

    @ParameterizedTest
    @MethodSource("denyLocationCases")
    void shouldHandleDenyPaths(String denyPath, String expectedPath) {
      properties.setDenyPath(denyPath);

      Optional<Location> location = configuration.getDenyLocation();

      assertTrue(location.isPresent());
      assertEquals(expectedPath, location.get().getFullURI());
    }

    private static Stream<Arguments> denyLocationCases() {
      return Stream.of(Arguments.of("/denied", "/denied"),
          Arguments.of("/error/403?type=authorization", "/error/403?type=authorization"));
    }

    @Test
    void shouldFallBackToAuthenticationLocationWhenDenyPathIsNotSet() {
      properties.setAuthenticationPath("/signin");

      Optional<Location> location = configuration.getDenyLocation();

      assertTrue(location.isPresent());
      assertEquals("/signin", location.get().getSegments().getPath());
      assertTrue(location.get().getQueryParameters().containsKey("error"));
    }
  }

  @Nested
  class LogoutLocation {

    @Test
    void shouldUseTheLogoutPathWhenItIsSet() {
      properties.setAuthenticationPath("/signin");
      properties.setLogoutSuccessPath("/goodbye");

      Optional<Location> location = configuration.getLogoutLocation();

      assertTrue(location.isPresent());
      assertEquals("/goodbye", location.get().getFullURI());
    }

    @Test
    void shouldUseTheAuthenticationPathWhenNoLogoutPathIsSet() {
      properties.setAuthenticationPath("/signin");

      Optional<Location> location = configuration.getLogoutLocation();

      assertTrue(location.isPresent());
      assertEquals("/signin", location.get().getFullURI());
    }

    @Test
    void shouldUseTheDefaultLoginPathWhenNeitherPathIsSet() {
      Optional<Location> location = configuration.getLogoutLocation();

      assertTrue(location.isPresent());
      assertEquals("/login", location.get().getFullURI());
    }
  }
}
