package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;
import java.util.stream.Stream;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;

class BBjConfigReaderTest {

  private final BBjRouteSecurityConfiguration defaults =
      new BBjRouteSecurityConfiguration(new BBjSecurityConfigurationProperties());

  @Test
  void shouldReadEveryKey() {
    Config config = ConfigFactory.parseString("""
        webforj.security {
          enabled = false
          secure-by-default = false
          authentication-path = "/signin"
          deny-path = "/denied"
        }
        """);

    BBjRouteSecurityConfiguration configuration = BBjConfigReader.readConfig(config);

    assertFalse(configuration.isEnabled());
    assertFalse(configuration.isSecureByDefault());
    assertEquals("/signin", configuration.getAuthenticationLocation().orElseThrow().getFullURI());
    assertEquals("/denied", configuration.getDenyLocation().orElseThrow().getFullURI());
  }

  @Nested
  class AbsentKeys {

    @Test
    void shouldKeepDefaultsWhenTheSubtreeIsAbsent() {
      Config config = ConfigFactory.empty();

      BBjRouteSecurityConfiguration configuration = BBjConfigReader.readConfig(config);

      assertTrue(configuration.isEnabled());
      assertTrue(configuration.isSecureByDefault());
      assertEquals(defaults.getAuthenticationLocation(), configuration.getAuthenticationLocation());
      assertEquals(defaults.getDenyLocation(), configuration.getDenyLocation());
    }

    @ParameterizedTest
    @MethodSource("partialConfigurationCases")
    void shouldKeepDefaultsForKeysThatAreAbsent(String hocon, boolean enabled,
        boolean secureByDefault) {
      Config config = ConfigFactory.parseString(hocon);

      BBjRouteSecurityConfiguration configuration = BBjConfigReader.readConfig(config);

      assertEquals(enabled, configuration.isEnabled());
      assertEquals(secureByDefault, configuration.isSecureByDefault());
    }

    private static Stream<Arguments> partialConfigurationCases() {
      return Stream.of(Arguments.of("webforj.security.enabled = false", false, true),
          Arguments.of("webforj.security.secure-by-default = false", true, false),
          Arguments.of("webforj.security.authentication-path = \"/signin\"", true, true));
    }

    @Test
    void shouldKeepPathDefaultsWhenOnlyOnePathIsSet() {
      Config config = ConfigFactory.parseString("webforj.security.deny-path = \"/denied\"");

      BBjRouteSecurityConfiguration configuration = BBjConfigReader.readConfig(config);

      assertEquals(defaults.getAuthenticationLocation(), configuration.getAuthenticationLocation());
      assertEquals("/denied", configuration.getDenyLocation().orElseThrow().getFullURI());
    }
  }

  @Nested
  class InvalidValues {

    @ParameterizedTest
    @ValueSource(strings = {"\"maybe\"", "\"\"", "5", "[true]"})
    void shouldTreatAnInvalidEnabledValueAsEnabled(String value) {
      Config config = ConfigFactory.parseString("webforj.security.enabled = " + value);

      BBjRouteSecurityConfiguration configuration = BBjConfigReader.readConfig(config);

      assertTrue(configuration.isEnabled());
    }
  }
}
