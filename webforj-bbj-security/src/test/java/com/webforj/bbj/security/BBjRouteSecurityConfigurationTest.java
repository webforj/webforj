package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;
import com.webforj.router.history.Location;
import com.webforj.router.security.RouteSecurityConfiguration;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.logging.Formatter;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;
import java.util.stream.Stream;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class BBjRouteSecurityConfigurationTest {
  @SuppressWarnings("LoggerInitializedWithForeignClass")
  private final Logger logger = Logger.getLogger(BBjRouteSecurityConfiguration.class.getName());
  private final List<String> warnings = new ArrayList<>();
  private final List<String> debugs = new ArrayList<>();
  private Level previousLevel;
  private final Handler handler = new Handler() {
    private final Formatter formatter = new SimpleFormatter();

    @Override
    public void publish(LogRecord record) {
      if (record.getLevel() == Level.WARNING) {
        warnings.add(formatter.formatMessage(record));
      } else if (record.getLevel() == Level.FINE) {
        debugs.add(formatter.formatMessage(record));
      }
    }

    @Override
    public void flush() {}

    @Override
    public void close() {}
  };

  @BeforeEach
  void setUp() {
    previousLevel = logger.getLevel();
    logger.setLevel(Level.ALL);
    logger.addHandler(handler);
  }

  @AfterEach
  void tearDown() {
    logger.removeHandler(handler);
    logger.setLevel(previousLevel);
  }

  @Test
  void shouldNotBeEnabledSilentlyWhenTheKeyIsMissing() {
    BBjRouteSecurityConfiguration configuration =
        BBjRouteSecurityConfiguration.create(ConfigFactory.empty());

    assertFalse(configuration.isEnabled());
    assertTrue(warnings.isEmpty());
    assertTrue(debugs.contains("webforj.security.enabled is not set, using false"));
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

  @Nested
  class EnabledKey {

    @ParameterizedTest
    @MethodSource("com.webforj.bbj.security.BBjRouteSecurityConfigurationTest#createBadBooleans")
    void shouldFailClosedWithOneWarningWhenTheValueIsInvalid(String value) {
      Config config = ConfigFactory.parseString("webforj.security.enabled = " + value);

      BBjRouteSecurityConfiguration configuration = BBjRouteSecurityConfiguration.create(config);

      assertTrue(configuration.isEnabled());
      assertEquals(1, warnings.size());
      assertTrue(warnings.getFirst().contains("webforj.security.enabled"));
    }

    @Test
    void shouldReadBooleanTextValues() {
      Config config = ConfigFactory.parseString("webforj.security.enabled = \"true\"");

      assertTrue(BBjRouteSecurityConfiguration.create(config).isEnabled());
      assertTrue(warnings.isEmpty());
    }
  }

  @Nested
  class SecureByDefaultKey {

    @Test
    void shouldBeSecureByDefaultWhenTheKeyIsMissing() {
      BBjRouteSecurityConfiguration configuration =
          BBjRouteSecurityConfiguration.create(ConfigFactory.empty());

      assertTrue(configuration.isSecureByDefault());
      assertTrue(warnings.isEmpty());
      assertTrue(debugs.contains("webforj.security.secure-by-default is not set, using true"));
    }

    @ParameterizedTest
    @MethodSource("com.webforj.bbj.security.BBjRouteSecurityConfigurationTest#createBooleans")
    void shouldReadTheValue(String value, boolean expected) {
      Config config = ConfigFactory.parseString("webforj.security.secure-by-default = " + value);

      assertEquals(expected, BBjRouteSecurityConfiguration.create(config).isSecureByDefault());
      assertTrue(warnings.isEmpty());
    }

    @ParameterizedTest
    @MethodSource("com.webforj.bbj.security.BBjRouteSecurityConfigurationTest#createBadBooleans")
    void shouldStaySecureWithOneWarningWhenTheValueIsInvalid(String value) {
      Config config = ConfigFactory.parseString("webforj.security.secure-by-default = " + value);

      assertTrue(BBjRouteSecurityConfiguration.create(config).isSecureByDefault());
      assertEquals(1, warnings.size());
      assertTrue(warnings.getFirst().contains("webforj.security.secure-by-default"));
    }
  }

  @Nested
  class AuthenticationPathKey {

    @Test
    void shouldUseTheDefaultSilentlyWhenTheKeyIsMissing() {
      BBjRouteSecurityConfiguration configuration =
          BBjRouteSecurityConfiguration.create(ConfigFactory.empty());

      assertEquals(new Location("/login"), configuration.getAuthenticationLocation().orElseThrow());
      assertTrue(warnings.isEmpty());
      assertTrue(debugs.contains("webforj.security.authentication-path is not set, using /login"));
    }

    @Test
    void shouldReadTheValue() {
      Config config =
          ConfigFactory.parseString("webforj.security.authentication-path = \"/signin\"");

      assertEquals(new Location("/signin"),
          BBjRouteSecurityConfiguration.create(config).getAuthenticationLocation().orElseThrow());
      assertTrue(warnings.isEmpty());
    }

    @ParameterizedTest
    @MethodSource("com.webforj.bbj.security.BBjRouteSecurityConfigurationTest#createInvalidPaths")
    void shouldUseTheDefaultWithOneWarningWhenTheValueIsInvalid(String value) {
      Config config = ConfigFactory.parseString("webforj.security.authentication-path = " + value);

      assertEquals(new Location("/login"),
          BBjRouteSecurityConfiguration.create(config).getAuthenticationLocation().orElseThrow());
      assertEquals(1, warnings.size());
      assertTrue(warnings.getFirst().contains("webforj.security.authentication-path"));
    }
  }

  @Nested
  class DenyPathKey {

    @Test
    void shouldFallBackToTheAuthenticationLocationWhenTheKeyIsMissing() {
      Config config =
          ConfigFactory.parseString("webforj.security.authentication-path = \"/signin\"");
      RouteSecurityConfiguration expected = new RouteSecurityConfiguration() {
        @Override
        public Optional<Location> getAuthenticationLocation() {
          return Optional.of(new Location("/signin"));
        }
      };

      assertEquals(expected.getDenyLocation(),
          BBjRouteSecurityConfiguration.create(config).getDenyLocation());
      assertTrue(warnings.isEmpty());
      assertEquals(1,
          debugs.stream()
              .filter(
                  message -> message.startsWith("webforj.security.deny-path is not set, using "))
              .count());
    }

    @Test
    void shouldReadTheValue() {
      Config config = ConfigFactory.parseString("webforj.security.deny-path = \"/access-denied\"");

      assertEquals(new Location("/access-denied"),
          BBjRouteSecurityConfiguration.create(config).getDenyLocation().orElseThrow());
      assertTrue(warnings.isEmpty());
    }

    @Test
    void shouldNotShareTheLocationBetweenCalls() {
      Config config = ConfigFactory.parseString("webforj.security.deny-path = \"/access-denied\"");
      BBjRouteSecurityConfiguration configuration = BBjRouteSecurityConfiguration.create(config);

      configuration.getDenyLocation().orElseThrow().getQueryParameters().put("reason", "x");

      assertEquals(new Location("/access-denied"), configuration.getDenyLocation().orElseThrow());
    }

    @ParameterizedTest
    @MethodSource("com.webforj.bbj.security.BBjRouteSecurityConfigurationTest#createInvalidPaths")
    void shouldUseTheDefaultWithOneWarningWhenTheValueIsInvalid(String value) {
      Config config = ConfigFactory.parseString("webforj.security.deny-path = " + value);

      assertEquals(new RouteSecurityConfiguration() {}.getDenyLocation(),
          BBjRouteSecurityConfiguration.create(config).getDenyLocation());
      assertEquals(1, warnings.size());
      assertTrue(warnings.getFirst().contains("webforj.security.deny-path"));
    }
  }

  @Nested
  class DefaultSuccessPathKey {

    @Test
    void shouldUseTheRootSilentlyWhenTheKeyIsMissing() {
      BBjRouteSecurityConfiguration configuration =
          BBjRouteSecurityConfiguration.create(ConfigFactory.empty());

      assertEquals(new Location("/"), configuration.getDefaultSuccessLocation());
      assertTrue(warnings.isEmpty());
      assertTrue(debugs.contains("webforj.security.bbj.default-success-path is not set, using /"));
    }

    @Test
    void shouldReadTheValue() {
      Config config =
          ConfigFactory.parseString("webforj.security.bbj.default-success-path = \"/home?tab=1\"");

      assertEquals(new Location("/home?tab=1"),
          BBjRouteSecurityConfiguration.create(config).getDefaultSuccessLocation());
      assertTrue(warnings.isEmpty());
    }

    @ParameterizedTest
    @MethodSource("com.webforj.bbj.security.BBjRouteSecurityConfigurationTest#createInvalidPaths")
    void shouldUseTheRootWithOneWarningWhenTheValueIsInvalid(String value) {
      Config config =
          ConfigFactory.parseString("webforj.security.bbj.default-success-path = " + value);

      assertEquals(new Location("/"),
          BBjRouteSecurityConfiguration.create(config).getDefaultSuccessLocation());
      assertEquals(1, warnings.size());
      assertTrue(warnings.getFirst().contains("webforj.security.bbj.default-success-path"));
    }

    @Test
    void shouldNotShareTheLocationBetweenCalls() {
      BBjRouteSecurityConfiguration configuration =
          BBjRouteSecurityConfiguration.create(ConfigFactory.empty());

      configuration.getDefaultSuccessLocation().getQueryParameters().put("reason", "x");

      assertEquals(new Location("/"), configuration.getDefaultSuccessLocation());
    }
  }

  @Nested
  class ReturnAfterLoginKey {

    @Test
    void shouldReturnSilentlyWhenTheKeyIsMissing() {
      BBjRouteSecurityConfiguration configuration =
          BBjRouteSecurityConfiguration.create(ConfigFactory.empty());

      assertTrue(configuration.isReturnAfterLogin());
      assertTrue(warnings.isEmpty());
      assertTrue(debugs.contains("webforj.security.bbj.return-after-login is not set, using true"));
    }

    @ParameterizedTest
    @MethodSource("com.webforj.bbj.security.BBjRouteSecurityConfigurationTest#createBooleans")
    void shouldReadTheValue(String value, boolean expected) {
      Config config =
          ConfigFactory.parseString("webforj.security.bbj.return-after-login = " + value);

      assertEquals(expected, BBjRouteSecurityConfiguration.create(config).isReturnAfterLogin());
      assertTrue(warnings.isEmpty());
    }

    @ParameterizedTest
    @MethodSource("com.webforj.bbj.security.BBjRouteSecurityConfigurationTest#createBadBooleans")
    void shouldReturnWithOneWarningWhenTheValueIsInvalid(String value) {
      Config config =
          ConfigFactory.parseString("webforj.security.bbj.return-after-login = " + value);

      assertTrue(BBjRouteSecurityConfiguration.create(config).isReturnAfterLogin());
      assertEquals(1, warnings.size());
      assertTrue(warnings.getFirst().contains("webforj.security.bbj.return-after-login"));
    }
  }

  @Nested
  class LogoutSuccessPathKey {

    @Test
    void shouldDeriveTheDefaultFromTheDefaultAuthenticationPath() {
      BBjRouteSecurityConfiguration configuration =
          BBjRouteSecurityConfiguration.create(ConfigFactory.empty());

      assertEquals(new Location("/login?logout"), configuration.getLogoutLocation());
      assertTrue(warnings.isEmpty());
      assertTrue(debugs.contains(
          "webforj.security.bbj.logout-success-path is not set, using the authentication path with a logout query parameter"));
    }

    @Test
    void shouldDeriveTheDefaultFromAConfiguredAuthenticationPath() {
      Config config =
          ConfigFactory.parseString("webforj.security.authentication-path = \"/signin\"");

      assertEquals(new Location("/signin?logout"),
          BBjRouteSecurityConfiguration.create(config).getLogoutLocation());
    }

    @Test
    void shouldReadTheValue() {
      Config config =
          ConfigFactory.parseString("webforj.security.authentication-path = \"/signin\"\n"
              + "webforj.security.bbj.logout-success-path = \"/public\"");

      assertEquals(new Location("/public"),
          BBjRouteSecurityConfiguration.create(config).getLogoutLocation());
      assertTrue(warnings.isEmpty());
    }

    @ParameterizedTest
    @MethodSource("com.webforj.bbj.security.BBjRouteSecurityConfigurationTest#createInvalidPaths")
    void shouldUseTheDerivedDefaultWithOneWarningWhenTheValueIsInvalid(String value) {
      Config config =
          ConfigFactory.parseString("webforj.security.bbj.logout-success-path = " + value);

      assertEquals(new Location("/login?logout"),
          BBjRouteSecurityConfiguration.create(config).getLogoutLocation());
      assertEquals(1, warnings.size());
      assertTrue(warnings.getFirst().contains("webforj.security.bbj.logout-success-path"));
    }
  }

  private static Stream<Arguments> createBooleans() {
    return Stream.of(Arguments.of("true", true), Arguments.of("false", false));
  }

  private static Stream<String> createBadBooleans() {
    return Stream.of("maybe", "[true]", "{a = 1}", "42", "${missing.value}");
  }

  private static Stream<String> createInvalidPaths() {
    return Stream.of("\"\"", "\"   \"", "\"login\"", "\"https://evil.example/x\"",
        "\"//evil.example\"", "\"/a\\\\b\"", "\"/a b\"", "[\"/login\"]", "{a = 1}",
        "${missing.value}");
  }
}
