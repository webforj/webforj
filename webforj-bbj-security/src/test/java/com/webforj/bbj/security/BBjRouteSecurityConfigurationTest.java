package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;
import com.webforj.router.security.RouteSecurityConfiguration;
import java.util.Map;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class BBjRouteSecurityConfigurationTest {

  @Test
  void shouldNotBeEnabledWhenTheKeyIsMissing() {
    BBjRouteSecurityConfiguration configuration =
        BBjRouteSecurityConfiguration.create(ConfigFactory.empty());

    assertFalse(configuration.isEnabled());
  }

  @Test
  void shouldKeepTheInterfaceDefaultsForTheOtherSettings() {
    BBjRouteSecurityConfiguration configuration =
        BBjRouteSecurityConfiguration.create(ConfigFactory.empty());
    RouteSecurityConfiguration defaults = new RouteSecurityConfiguration() {};

    assertEquals(defaults.isSecureByDefault(), configuration.isSecureByDefault());
    assertEquals(defaults.getAuthenticationLocation(), configuration.getAuthenticationLocation());
    assertEquals(defaults.getDenyLocation(), configuration.getDenyLocation());
  }

  @ParameterizedTest
  @MethodSource("createSwitchCases")
  void shouldReadTheMasterSwitch(Config config, boolean expected) {
    BBjRouteSecurityConfiguration configuration = BBjRouteSecurityConfiguration.create(config);

    assertEquals(expected, configuration.isEnabled());
  }

  private static Stream<Arguments> createSwitchCases() {
    return Stream.of(
        Arguments.of(ConfigFactory.parseMap(Map.of("webforj.security.enabled", true)), true),
        Arguments.of(ConfigFactory.parseMap(Map.of("webforj.security.enabled", false)), false),
        Arguments.of(ConfigFactory.parseString("webforj.security.enabled = null"), false));
  }
}
