package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;
import java.util.ArrayList;
import java.util.List;
import java.util.function.ToIntFunction;
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
import org.junit.jupiter.params.provider.ValueSource;

class BBjSecurityConfigurationPropertiesTest {

  private BBjSecurityConfigurationProperties properties;

  @BeforeEach
  void setUp() {
    properties = new BBjSecurityConfigurationProperties(ConfigFactory.empty());
  }

  @Nested
  class DefaultValues {

    @Test
    void shouldHaveDefaultEnabledValue() {
      assertTrue(properties.getEnabled());
    }

    @Test
    void shouldHaveDefaultSecureByDefaultValue() {
      assertTrue(properties.getSecureByDefault());
    }

    @Test
    void shouldHaveNullAuthenticationPath() {
      assertNull(properties.getAuthenticationPath());
    }

    @Test
    void shouldHaveNullDenyPath() {
      assertNull(properties.getDenyPath());
    }

    @Test
    void shouldHaveNullLogoutSuccessPath() {
      assertNull(properties.getLogoutSuccessPath());
    }

    @Test
    void shouldHaveDefaultAdminHost() {
      assertEquals("127.0.0.1", properties.getAdminHost());
    }

    @Test
    void shouldHaveDefaultAdminPort() {
      assertEquals(2002, properties.getAdminPort());
    }

    @Test
    void shouldHaveDefaultAdminTimeoutSeconds() {
      assertEquals(10, properties.getAdminTimeoutSeconds());
    }

    @Test
    void shouldHaveDefaultTokenMinutes() {
      assertEquals(30, properties.getTokenMinutes());
    }

    @Test
    void shouldHaveDefaultSuccessPath() {
      assertEquals("/", properties.getDefaultSuccessPath());
    }

    @Test
    void shouldHaveDefaultReturnAfterLogin() {
      assertTrue(properties.getReturnAfterLogin());
    }
  }

  @Nested
  class PropertySetters {

    @Test
    void shouldSetEnabled() {
      properties.setEnabled(false);
      assertFalse(properties.getEnabled());
    }

    @Test
    void shouldSetSecureByDefault() {
      properties.setSecureByDefault(false);
      assertFalse(properties.getSecureByDefault());
    }

    @Test
    void shouldSetAuthenticationPath() {
      properties.setAuthenticationPath("/signin");
      assertEquals("/signin", properties.getAuthenticationPath());
    }

    @Test
    void shouldSetDenyPath() {
      properties.setDenyPath("/access-denied");
      assertEquals("/access-denied", properties.getDenyPath());
    }

    @Test
    void shouldSetLogoutSuccessPath() {
      properties.setLogoutSuccessPath("/goodbye");
      assertEquals("/goodbye", properties.getLogoutSuccessPath());
    }

    @Test
    void shouldSetAdminHost() {
      properties.setAdminHost("bbj.example.com");
      assertEquals("bbj.example.com", properties.getAdminHost());
    }

    @Test
    void shouldSetAdminPort() {
      properties.setAdminPort(2020);
      assertEquals(2020, properties.getAdminPort());
    }

    @Test
    void shouldSetAdminTimeoutSeconds() {
      properties.setAdminTimeoutSeconds(5);
      assertEquals(5, properties.getAdminTimeoutSeconds());
    }

    @Test
    void shouldSetTokenMinutes() {
      properties.setTokenMinutes(15);
      assertEquals(15, properties.getTokenMinutes());
    }

    @Test
    void shouldSetDefaultSuccessPath() {
      properties.setDefaultSuccessPath("/home");
      assertEquals("/home", properties.getDefaultSuccessPath());
    }

    @Test
    void shouldSetReturnAfterLogin() {
      properties.setReturnAfterLogin(false);
      assertFalse(properties.getReturnAfterLogin());
    }
  }

  @Nested
  class ReadValues {

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

      properties = new BBjSecurityConfigurationProperties(config);

      assertFalse(properties.getEnabled());
      assertFalse(properties.getSecureByDefault());
      assertEquals("/signin", properties.getAuthenticationPath());
      assertEquals("/denied", properties.getDenyPath());
    }

    @Test
    void shouldReadEveryAdminKey() {
      Config config = ConfigFactory.parseString("""
          webforj.security.bbj {
            admin {
              host = "bbj.example.com"
              port = 2020
              timeout-seconds = 5
            }
            token.minutes = 15
            default-success-path = "/home"
            return-after-login = false
            logout-success-path = "/goodbye"
          }
          """);

      properties = new BBjSecurityConfigurationProperties(config);

      assertEquals("bbj.example.com", properties.getAdminHost());
      assertEquals(2020, properties.getAdminPort());
      assertEquals(5, properties.getAdminTimeoutSeconds());
      assertEquals(15, properties.getTokenMinutes());
      assertEquals("/home", properties.getDefaultSuccessPath());
      assertFalse(properties.getReturnAfterLogin());
      assertEquals("/goodbye", properties.getLogoutSuccessPath());
    }
  }

  @Nested
  class IntegerValues {

    @ParameterizedTest
    @MethodSource("acceptedIntegerCases")
    void shouldReadTheIntegerFromTheValue(String value, int expected) {
      Config config = ConfigFactory.parseString("webforj.security.bbj.admin.port = " + value);

      properties = new BBjSecurityConfigurationProperties(config);

      assertEquals(expected, properties.getAdminPort());
    }

    private static Stream<Arguments> acceptedIntegerCases() {
      return Stream.of(Arguments.of("2020", 2020), Arguments.of("\"2020\"", 2020),
          Arguments.of("2020.0", 2020));
    }

    @ParameterizedTest
    @MethodSource("acceptedBoundCases")
    void shouldReadTheIntegerAtTheBoundsOfItsRange(String key, int value,
        ToIntFunction<BBjSecurityConfigurationProperties> getter) {
      Config config =
          ConfigFactory.parseString("webforj.security.bbj.%s = %d".formatted(key, value));

      properties = new BBjSecurityConfigurationProperties(config);

      assertEquals(value, getter.applyAsInt(properties));
    }

    private static Stream<Arguments> acceptedBoundCases() {
      ToIntFunction<BBjSecurityConfigurationProperties> port =
          BBjSecurityConfigurationProperties::getAdminPort;
      ToIntFunction<BBjSecurityConfigurationProperties> timeout =
          BBjSecurityConfigurationProperties::getAdminTimeoutSeconds;
      ToIntFunction<BBjSecurityConfigurationProperties> token =
          BBjSecurityConfigurationProperties::getTokenMinutes;

      return Stream.of(Arguments.of("admin.port", 1, port), Arguments.of("admin.port", 40000, port),
          Arguments.of("admin.port", 65535, port),
          Arguments.of("admin.timeout-seconds", 1, timeout),
          Arguments.of("admin.timeout-seconds", 9999, timeout),
          Arguments.of("token.minutes", 1, token), Arguments.of("token.minutes", 480, token));
    }
  }

  @Nested
  class AbsentKeys {

    @Test
    void shouldKeepDefaultsWhenTheSubtreeIsAbsent() {
      Config config = ConfigFactory.empty();

      properties = new BBjSecurityConfigurationProperties(config);

      assertTrue(properties.getEnabled());
      assertTrue(properties.getSecureByDefault());
      assertNull(properties.getAuthenticationPath());
      assertNull(properties.getDenyPath());
    }

    @ParameterizedTest
    @MethodSource("partialConfigurationCases")
    void shouldKeepDefaultsForKeysThatAreAbsent(String hocon, boolean enabled,
        boolean secureByDefault) {
      Config config = ConfigFactory.parseString(hocon);

      properties = new BBjSecurityConfigurationProperties(config);

      assertEquals(enabled, properties.getEnabled());
      assertEquals(secureByDefault, properties.getSecureByDefault());
    }

    private static Stream<Arguments> partialConfigurationCases() {
      return Stream.of(Arguments.of("webforj.security.enabled = false", false, true),
          Arguments.of("webforj.security.secure-by-default = false", true, false),
          Arguments.of("webforj.security.authentication-path = \"/signin\"", true, true));
    }

    @Test
    void shouldKeepPathDefaultsWhenOnlyOnePathIsSet() {
      Config config = ConfigFactory.parseString("webforj.security.deny-path = \"/denied\"");

      properties = new BBjSecurityConfigurationProperties(config);

      assertNull(properties.getAuthenticationPath());
      assertEquals("/denied", properties.getDenyPath());
    }

    @Test
    void shouldKeepTheDefaultsForEveryKeyThatIsAbsent() {
      Config config = ConfigFactory.empty();

      properties = new BBjSecurityConfigurationProperties(config);

      assertTrue(properties.getEnabled());
      assertTrue(properties.getSecureByDefault());
      assertNull(properties.getAuthenticationPath());
      assertNull(properties.getDenyPath());
    }

    @Test
    void shouldKeepTheAdminDefaultsWhenTheSubtreeIsAbsent() {
      Config config = ConfigFactory.empty();

      properties = new BBjSecurityConfigurationProperties(config);

      assertEquals("127.0.0.1", properties.getAdminHost());
      assertEquals(2002, properties.getAdminPort());
      assertEquals(10, properties.getAdminTimeoutSeconds());
      assertEquals(30, properties.getTokenMinutes());
      assertEquals("/", properties.getDefaultSuccessPath());
      assertTrue(properties.getReturnAfterLogin());
    }

    @Test
    void shouldKeepTheAdminDefaultsForEveryKeyThatIsAbsent() {
      Config config = ConfigFactory.empty();

      properties = new BBjSecurityConfigurationProperties(config);

      assertEquals("127.0.0.1", properties.getAdminHost());
      assertEquals(2002, properties.getAdminPort());
      assertEquals(10, properties.getAdminTimeoutSeconds());
      assertEquals(30, properties.getTokenMinutes());
      assertEquals("/", properties.getDefaultSuccessPath());
      assertTrue(properties.getReturnAfterLogin());
    }

    @Test
    void shouldKeepTheOtherAdminValuesWhenOnlyOneIsSet() {
      Config config = ConfigFactory.parseString("webforj.security.bbj.admin.port = 2020");

      properties = new BBjSecurityConfigurationProperties(config);

      assertEquals("127.0.0.1", properties.getAdminHost());
      assertEquals(2020, properties.getAdminPort());
      assertEquals(10, properties.getAdminTimeoutSeconds());
    }
  }

  @Nested
  class ThrottleKeys {

    @Test
    void shouldHaveTheThrottleDefaults() {
      assertEquals(5, properties.getThrottleMaxAttempts());
      assertEquals(20, properties.getThrottleAddressMaxAttempts());
      assertEquals(300, properties.getThrottleWindowSeconds());
    }

    @Test
    void shouldSetTheThrottleValues() {
      properties.setThrottleMaxAttempts(3);
      properties.setThrottleAddressMaxAttempts(0);
      properties.setThrottleWindowSeconds(60);

      assertEquals(3, properties.getThrottleMaxAttempts());
      assertEquals(0, properties.getThrottleAddressMaxAttempts());
      assertEquals(60, properties.getThrottleWindowSeconds());
    }

    @Test
    void shouldReadEveryThrottleKey() {
      Config config = ConfigFactory.parseString("""
          webforj.security.bbj.throttle {
            max-attempts = 3
            address-max-attempts = 6
            window-seconds = 60
          }
          """);

      properties = new BBjSecurityConfigurationProperties(config);

      assertEquals(3, properties.getThrottleMaxAttempts());
      assertEquals(6, properties.getThrottleAddressMaxAttempts());
      assertEquals(60, properties.getThrottleWindowSeconds());
    }

    @ParameterizedTest
    @MethodSource("throttleBoundCases")
    void shouldReadTheThrottleKeyAtTheBoundsOfItsRange(String key, int value,
        ToIntFunction<BBjSecurityConfigurationProperties> getter) {
      Config config =
          ConfigFactory.parseString("webforj.security.bbj.throttle.%s = %d".formatted(key, value));

      properties = new BBjSecurityConfigurationProperties(config);

      assertEquals(value, getter.applyAsInt(properties));
    }

    private static Stream<Arguments> throttleBoundCases() {
      ToIntFunction<BBjSecurityConfigurationProperties> user =
          BBjSecurityConfigurationProperties::getThrottleMaxAttempts;
      ToIntFunction<BBjSecurityConfigurationProperties> address =
          BBjSecurityConfigurationProperties::getThrottleAddressMaxAttempts;
      ToIntFunction<BBjSecurityConfigurationProperties> window =
          BBjSecurityConfigurationProperties::getThrottleWindowSeconds;

      return Stream.of(Arguments.of("max-attempts", 1, user),
          Arguments.of("max-attempts", 1000, user),
          Arguments.of("address-max-attempts", 0, address),
          Arguments.of("address-max-attempts", 10000, address),
          Arguments.of("window-seconds", 10, window),
          Arguments.of("window-seconds", 86400, window));
    }

    @ParameterizedTest
    @MethodSource("invalidThrottleValues")
    void shouldKeepTheThrottleDefaultsForInvalidValues(String key, String value) {
      Config config =
          ConfigFactory.parseString("webforj.security.bbj.throttle.%s = %s".formatted(key, value));

      properties = new BBjSecurityConfigurationProperties(config);

      assertEquals(5, properties.getThrottleMaxAttempts());
      assertEquals(20, properties.getThrottleAddressMaxAttempts());
      assertEquals(300, properties.getThrottleWindowSeconds());
    }

    private static Stream<Arguments> invalidThrottleValues() {
      return Stream.of(Arguments.of("max-attempts", "0"), Arguments.of("max-attempts", "1001"),
          Arguments.of("address-max-attempts", "-1"), Arguments.of("address-max-attempts", "10001"),
          Arguments.of("window-seconds", "9"), Arguments.of("window-seconds", "86401"),
          Arguments.of("max-attempts", "\"abc\""), Arguments.of("address-max-attempts", "2.5"),
          Arguments.of("window-seconds", "true"));
    }
  }

  @Nested
  class InvalidValues {
    private final Logger logger =
        Logger.getLogger(BBjSecurityConfigurationProperties.class.getName());
    private final List<LogRecord> records = new ArrayList<>();
    private final Handler handler = new Handler() {
      @Override
      public void publish(LogRecord logRecord) {
        records.add(logRecord);
      }

      @Override
      public void flush() {
        // Not implemented
      }

      @Override
      public void close() {
        // Not implemented
      }
    };
    private Level previousLevel;

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

    @ParameterizedTest
    @ValueSource(strings = {"\"maybe\"", "\"\"", "5", "[true]"})
    void shouldTreatAnInvalidEnabledValueAsEnabled(String value) {
      Config config = ConfigFactory.parseString("webforj.security.enabled = " + value);

      properties = new BBjSecurityConfigurationProperties(config);

      assertTrue(properties.getEnabled());
    }

    @ParameterizedTest
    @ValueSource(strings = {"\"maybe\"", "\"\"", "5", "[true]"})
    void shouldTreatAnInvalidSecureByDefaultValueAsEnabled(String value) {
      Config config = ConfigFactory.parseString("webforj.security.secure-by-default = " + value);

      properties = new BBjSecurityConfigurationProperties(config);

      assertTrue(properties.getSecureByDefault());
    }

    @ParameterizedTest
    @MethodSource("invalidBooleanKeys")
    void shouldLogTheKeyWhenTheValueIsNotBoolean(String key) {
      Config config = ConfigFactory.parseString("webforj.security.%s = 5".formatted(key));

      properties = new BBjSecurityConfigurationProperties(config);

      List<LogRecord> warnings = getWarnings();
      assertEquals(1, warnings.size());
      assertEquals("The configuration key webforj.security.%s is not a boolean and counts as true"
          .formatted(key), new SimpleFormatter().formatMessage(warnings.get(0)));
    }

    @ParameterizedTest
    @MethodSource("invalidPathValues")
    void shouldKeepTheDefaultPathWhenItIsNotString(String key, String value) {
      Config config = ConfigFactory.parseString("webforj.security.%s = %s".formatted(key, value));

      properties = new BBjSecurityConfigurationProperties(config);

      assertNull(properties.getAuthenticationPath());
      assertNull(properties.getDenyPath());
    }

    @ParameterizedTest
    @MethodSource("invalidPathValues")
    void shouldLogTheKeyWhenPathIsNotString(String key, String value) {
      Config config = ConfigFactory.parseString("webforj.security.%s = %s".formatted(key, value));

      properties = new BBjSecurityConfigurationProperties(config);

      List<LogRecord> warnings = getWarnings();
      assertEquals(1, warnings.size());
      assertEquals("The configuration key webforj.security.%s is not a string and keeps its default"
          .formatted(key), new SimpleFormatter().formatMessage(warnings.get(0)));
    }

    @ParameterizedTest
    @MethodSource("invalidIntegerValues")
    void shouldKeepTheDefaultIntegerWhenItIsNotAnInteger(String key, String value) {
      Config config =
          ConfigFactory.parseString("webforj.security.bbj.%s = %s".formatted(key, value));

      properties = new BBjSecurityConfigurationProperties(config);

      assertEquals(2002, properties.getAdminPort());
      assertEquals(10, properties.getAdminTimeoutSeconds());
      assertEquals(30, properties.getTokenMinutes());
    }

    @ParameterizedTest
    @MethodSource("invalidIntegerValues")
    void shouldLogTheKeyWhenAnIntegerIsNotAnInteger(String key, String value) {
      Config config =
          ConfigFactory.parseString("webforj.security.bbj.%s = %s".formatted(key, value));

      properties = new BBjSecurityConfigurationProperties(config);

      List<LogRecord> warnings = getWarnings();
      assertEquals(1, warnings.size());
      assertEquals(
          "The configuration key webforj.security.bbj.%s is not an integer and keeps its default"
              .formatted(key),
          new SimpleFormatter().formatMessage(warnings.get(0)));
    }

    @ParameterizedTest
    @MethodSource("outOfRangeIntegerValues")
    void shouldKeepTheDefaultIntegerWhenItIsOutOfRange(String key, int value, int min, int max) {
      Config config =
          ConfigFactory.parseString("webforj.security.bbj.%s = %d".formatted(key, value));

      properties = new BBjSecurityConfigurationProperties(config);

      assertEquals(2002, properties.getAdminPort());
      assertEquals(10, properties.getAdminTimeoutSeconds());
      assertEquals(30, properties.getTokenMinutes());
    }

    @ParameterizedTest
    @MethodSource("outOfRangeIntegerValues")
    void shouldLogTheKeyWhenAnIntegerIsOutOfRange(String key, int value, int min, int max) {
      Config config =
          ConfigFactory.parseString("webforj.security.bbj.%s = %d".formatted(key, value));

      properties = new BBjSecurityConfigurationProperties(config);

      List<LogRecord> warnings = getWarnings();
      assertEquals(1, warnings.size());
      assertEquals(
          "The configuration key webforj.security.bbj.%s is not between %d and %d and keeps its "
              .formatted(key, min, max) + "default",
          new SimpleFormatter().formatMessage(warnings.get(0)));
    }

    @ParameterizedTest
    @ValueSource(strings = {"\"maybe\"", "\"\"", "5", "[true]"})
    void shouldTreatAnInvalidReturnAfterLoginValueAsEnabled(String value) {
      Config config =
          ConfigFactory.parseString("webforj.security.bbj.return-after-login = " + value);

      properties = new BBjSecurityConfigurationProperties(config);

      assertTrue(properties.getReturnAfterLogin());
    }

    @Test
    void shouldLogTheKeyWhenReturnAfterLoginIsNotBoolean() {
      Config config = ConfigFactory.parseString("webforj.security.bbj.return-after-login = 5");

      properties = new BBjSecurityConfigurationProperties(config);

      List<LogRecord> warnings = getWarnings();
      assertEquals(1, warnings.size());
      assertEquals(
          "The configuration key webforj.security.bbj.return-after-login is not a boolean and "
              + "counts as true",
          new SimpleFormatter().formatMessage(warnings.get(0)));
    }

    @ParameterizedTest
    @MethodSource("invalidAdminStringValues")
    void shouldKeepTheDefaultAdminStringWhenItIsNotString(String key, String value) {
      Config config =
          ConfigFactory.parseString("webforj.security.bbj.%s = %s".formatted(key, value));

      properties = new BBjSecurityConfigurationProperties(config);

      assertEquals("127.0.0.1", properties.getAdminHost());
      assertEquals("/", properties.getDefaultSuccessPath());
    }

    @ParameterizedTest
    @MethodSource("invalidAdminStringValues")
    void shouldLogTheKeyWhenAnAdminStringIsNotString(String key, String value) {
      Config config =
          ConfigFactory.parseString("webforj.security.bbj.%s = %s".formatted(key, value));

      properties = new BBjSecurityConfigurationProperties(config);

      List<LogRecord> warnings = getWarnings();
      assertEquals(1, warnings.size());
      assertEquals(
          "The configuration key webforj.security.bbj.%s is not a string and keeps its default"
              .formatted(key),
          new SimpleFormatter().formatMessage(warnings.get(0)));
    }

    @ParameterizedTest
    @MethodSource("invalidLogoutSuccessPathValues")
    void shouldKeepTheDefaultLogoutSuccessPathWhenItIsNotString(String value) {
      Config config =
          ConfigFactory.parseString("webforj.security.bbj.logout-success-path = " + value);

      properties = new BBjSecurityConfigurationProperties(config);

      assertNull(properties.getLogoutSuccessPath());
    }

    @ParameterizedTest
    @MethodSource("invalidLogoutSuccessPathValues")
    void shouldLogTheKeyWhenTheLogoutSuccessPathIsNotString(String value) {
      Config config =
          ConfigFactory.parseString("webforj.security.bbj.logout-success-path = " + value);

      properties = new BBjSecurityConfigurationProperties(config);

      List<LogRecord> warnings = getWarnings();
      assertEquals(1, warnings.size());
      assertEquals("The configuration key webforj.security.bbj.logout-success-path is not a string"
          + " and keeps its default", new SimpleFormatter().formatMessage(warnings.get(0)));
    }

    private List<LogRecord> getWarnings() {
      return records.stream().filter(it -> it.getLevel() == Level.WARNING).toList();
    }

    private static Stream<Arguments> invalidBooleanKeys() {
      return Stream.of(Arguments.of("enabled"), Arguments.of("secure-by-default"));
    }

    private static Stream<Arguments> invalidIntegerValues() {
      return Stream.of("admin.port", "admin.timeout-seconds", "token.minutes")
          .flatMap(key -> Stream.of("true", "[1]", "{a = 1}", "2.5", "\"abc\"", "\"\"")
              .map(value -> Arguments.of(key, value)));
    }

    private static Stream<Arguments> outOfRangeIntegerValues() {
      return Stream.of(Arguments.of("admin.port", 0, 1, 65535),
          Arguments.of("admin.port", 65536, 1, 65535), Arguments.of("admin.port", -1, 1, 65535),
          Arguments.of("admin.timeout-seconds", 0, 1, 9999),
          Arguments.of("admin.timeout-seconds", 10000, 1, 9999),
          Arguments.of("token.minutes", 0, 1, 480), Arguments.of("token.minutes", 481, 1, 480));
    }

    private static Stream<Arguments> invalidAdminStringValues() {
      return Stream.of("admin.host", "default-success-path").flatMap(key -> Stream
          .of("[\"a\"]", "{a = 1}", "5", "true").map(value -> Arguments.of(key, value)));
    }

    private static Stream<Arguments> invalidLogoutSuccessPathValues() {
      return Stream.of(Arguments.of("[\"/goodbye\"]"), Arguments.of("{a = 1}"), Arguments.of("5"),
          Arguments.of("true"));
    }

    private static Stream<Arguments> invalidPathValues() {
      return Stream.of(Arguments.of("authentication-path", "[\"/signin\"]"),
          Arguments.of("authentication-path", "{a = 1}"), Arguments.of("authentication-path", "5"),
          Arguments.of("authentication-path", "true"), Arguments.of("deny-path", "[\"/denied\"]"),
          Arguments.of("deny-path", "{a = 1}"), Arguments.of("deny-path", "5"),
          Arguments.of("deny-path", "true"));
    }
  }
}
