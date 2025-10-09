package com.webforj.spring.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

import com.webforj.router.history.Location;
import java.util.Optional;
import java.util.stream.Stream;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class SpringRouteSecurityConfigurationTest {

  @InjectMocks
  private SpringRouteSecurityConfiguration configuration;

  @Mock
  private SpringSecurityConfigurationProperties properties;

  @Nested
  class EnabledStatus {

    @ParameterizedTest
    @MethodSource("enabledStatusTestCases")
    void shouldHandleEnabledStatus(Boolean enabled, boolean expected) {
      when(properties.getEnabled()).thenReturn(enabled);
      assertEquals(expected, configuration.isEnabled());
    }

    static Stream<Arguments> enabledStatusTestCases() {
      return Stream.of(Arguments.of(Boolean.TRUE, true), Arguments.of(Boolean.FALSE, false),
          Arguments.of(null, false));
    }
  }

  @Nested
  class SecureByDefaultStatus {

    @ParameterizedTest
    @MethodSource("secureByDefaultTestCases")
    void shouldHandleSecureByDefaultStatus(Boolean secureByDefault, boolean expected) {
      when(properties.getSecureByDefault()).thenReturn(secureByDefault);
      assertEquals(expected, configuration.isSecureByDefault());
    }

    static Stream<Arguments> secureByDefaultTestCases() {
      return Stream.of(Arguments.of(Boolean.TRUE, true), Arguments.of(Boolean.FALSE, false),
          Arguments.of(null, false));
    }
  }

  @Nested
  class AuthenticationLocation {

    @ParameterizedTest
    @MethodSource("authenticationLocationTestCases")
    void shouldHandleAuthenticationPaths(String configuredPath, String expectedPath) {
      when(properties.getAuthenticationPath()).thenReturn(configuredPath);

      Optional<Location> location = configuration.getAuthenticationLocation();

      assertTrue(location.isPresent());
      assertEquals(expectedPath, location.get().toString());
    }

    static Stream<Arguments> authenticationLocationTestCases() {
      return Stream.of(Arguments.of("/signin", "/signin"), Arguments.of(null, "/login"),
          Arguments.of("/auth?redirect=true", "/auth?redirect=true"),
          Arguments.of("/auth#login-form", "/auth#login-form"));
    }
  }

  @Nested
  class DenyLocation {

    @ParameterizedTest
    @MethodSource("denyLocationTestCases")
    void shouldHandleDenyPaths(String denyPath, String expectedPath) {
      when(properties.getDenyPath()).thenReturn(denyPath);

      Optional<Location> location = configuration.getDenyLocation();

      assertTrue(location.isPresent());
      assertEquals(expectedPath, location.get().toString());
    }

    static Stream<Arguments> denyLocationTestCases() {
      return Stream.of(Arguments.of("/access-denied", "/access-denied"), Arguments.of("", "/"),
          Arguments.of("/error/403?type=authorization", "/error/403?type=authorization"));
    }

    @Test
    void shouldReturnSuperDefaultWhenDenyPathIsNull() {
      when(properties.getDenyPath()).thenReturn(null);
      when(properties.getAuthenticationPath()).thenReturn("/login");

      Optional<Location> location = configuration.getDenyLocation();

      assertTrue(location.isPresent());
      assertTrue(location.get().toString().contains("/login"));
    }
  }
}
