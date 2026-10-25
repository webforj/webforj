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

class BBjConfigReaderTest {

  private final BBjSecurityConfigurationProperties properties =
      new BBjSecurityConfigurationProperties();

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

    BBjConfigReader.readConfig(properties, config);

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
        }
        """);

    BBjConfigReader.readConfig(properties, config);

    assertEquals("bbj.example.com", properties.getAdminHost());
    assertEquals(2020, properties.getAdminPort());
    assertEquals(5, properties.getAdminTimeoutSeconds());
    assertEquals(15, properties.getTokenMinutes());
    assertEquals("/home", properties.getDefaultSuccessPath());
    assertFalse(properties.getReturnAfterLogin());
  }

  @Nested
  class IntegerValues {

    @ParameterizedTest
    @MethodSource("acceptedIntegerCases")
    void shouldReadTheIntegerFromTheValue(String value, int expected) {
      Config config = ConfigFactory.parseString("webforj.security.bbj.admin.port = " + value);

      BBjConfigReader.readConfig(properties, config);

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

      BBjConfigReader.readConfig(properties, config);

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

      BBjConfigReader.readConfig(properties, config);

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

      BBjConfigReader.readConfig(properties, config);

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

      BBjConfigReader.readConfig(properties, config);

      assertNull(properties.getAuthenticationPath());
      assertEquals("/denied", properties.getDenyPath());
    }

    @Test
    void shouldKeepTheValuesSetBeforeTheCallForEveryKeyThatIsAbsent() {
      properties.setEnabled(false);
      properties.setSecureByDefault(false);
      properties.setAuthenticationPath("/before-signin");
      properties.setDenyPath("/before-denied");
      Config config = ConfigFactory.empty();

      BBjConfigReader.readConfig(properties, config);

      assertFalse(properties.getEnabled());
      assertFalse(properties.getSecureByDefault());
      assertEquals("/before-signin", properties.getAuthenticationPath());
      assertEquals("/before-denied", properties.getDenyPath());
    }

    @Test
    void shouldKeepTheAdminDefaultsWhenTheSubtreeIsAbsent() {
      Config config = ConfigFactory.empty();

      BBjConfigReader.readConfig(properties, config);

      assertEquals("127.0.0.1", properties.getAdminHost());
      assertEquals(2002, properties.getAdminPort());
      assertEquals(10, properties.getAdminTimeoutSeconds());
      assertEquals(30, properties.getTokenMinutes());
      assertEquals("/", properties.getDefaultSuccessPath());
      assertTrue(properties.getReturnAfterLogin());
    }

    @Test
    void shouldKeepTheAdminValuesSetBeforeTheCallForEveryKeyThatIsAbsent() {
      properties.setAdminHost("before.example.com");
      properties.setAdminPort(1);
      properties.setAdminTimeoutSeconds(2);
      properties.setTokenMinutes(3);
      properties.setDefaultSuccessPath("/before-home");
      properties.setReturnAfterLogin(false);
      Config config = ConfigFactory.empty();

      BBjConfigReader.readConfig(properties, config);

      assertEquals("before.example.com", properties.getAdminHost());
      assertEquals(1, properties.getAdminPort());
      assertEquals(2, properties.getAdminTimeoutSeconds());
      assertEquals(3, properties.getTokenMinutes());
      assertEquals("/before-home", properties.getDefaultSuccessPath());
      assertFalse(properties.getReturnAfterLogin());
    }

    @Test
    void shouldKeepTheOtherAdminValuesWhenOnlyOneIsSet() {
      Config config = ConfigFactory.parseString("webforj.security.bbj.admin.port = 2020");

      BBjConfigReader.readConfig(properties, config);

      assertEquals("127.0.0.1", properties.getAdminHost());
      assertEquals(2020, properties.getAdminPort());
      assertEquals(10, properties.getAdminTimeoutSeconds());
    }
  }

  @Nested
  class InvalidValues {
    private final Logger logger = Logger.getLogger(BBjConfigReader.class.getName());
    private final List<LogRecord> records = new ArrayList<>();
    private final Handler handler = new Handler() {
      @Override
      public void publish(LogRecord record) {
        records.add(record);
      }

      @Override
      public void flush() {}

      @Override
      public void close() {}
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

      BBjConfigReader.readConfig(properties, config);

      assertTrue(properties.getEnabled());
    }

    @ParameterizedTest
    @ValueSource(strings = {"\"maybe\"", "\"\"", "5", "[true]"})
    void shouldTreatAnInvalidSecureByDefaultValueAsEnabled(String value) {
      properties.setSecureByDefault(false);
      Config config = ConfigFactory.parseString("webforj.security.secure-by-default = " + value);

      BBjConfigReader.readConfig(properties, config);

      assertTrue(properties.getSecureByDefault());
    }

    @ParameterizedTest
    @MethodSource("invalidBooleanKeys")
    void shouldLogTheKeyWhenABooleanIsNotABoolean(String key) {
      Config config = ConfigFactory.parseString("webforj.security.%s = 5".formatted(key));

      BBjConfigReader.readConfig(properties, config);

      List<LogRecord> warnings = getWarnings();
      assertEquals(1, warnings.size());
      assertEquals("The configuration key webforj.security.%s is not a boolean and counts as true"
          .formatted(key), new SimpleFormatter().formatMessage(warnings.get(0)));
    }

    @ParameterizedTest
    @MethodSource("invalidPathValues")
    void shouldKeepThePreviousPathWhenItIsNotAString(String key, String value) {
      properties.setAuthenticationPath("/before-signin");
      properties.setDenyPath("/before-denied");
      Config config = ConfigFactory.parseString("webforj.security.%s = %s".formatted(key, value));

      BBjConfigReader.readConfig(properties, config);

      assertEquals("/before-signin", properties.getAuthenticationPath());
      assertEquals("/before-denied", properties.getDenyPath());
    }

    @ParameterizedTest
    @MethodSource("invalidPathValues")
    void shouldLogTheKeyWhenAPathIsNotAString(String key, String value) {
      Config config = ConfigFactory.parseString("webforj.security.%s = %s".formatted(key, value));

      BBjConfigReader.readConfig(properties, config);

      List<LogRecord> warnings = getWarnings();
      assertEquals(1, warnings.size());
      assertEquals("The configuration key webforj.security.%s is not a string and keeps its default"
          .formatted(key), new SimpleFormatter().formatMessage(warnings.get(0)));
    }

    @ParameterizedTest
    @MethodSource("invalidIntegerValues")
    void shouldKeepThePreviousIntegerWhenItIsNotAnInteger(String key, String value) {
      properties.setAdminPort(11);
      properties.setAdminTimeoutSeconds(12);
      properties.setTokenMinutes(13);
      Config config =
          ConfigFactory.parseString("webforj.security.bbj.%s = %s".formatted(key, value));

      BBjConfigReader.readConfig(properties, config);

      assertEquals(11, properties.getAdminPort());
      assertEquals(12, properties.getAdminTimeoutSeconds());
      assertEquals(13, properties.getTokenMinutes());
    }

    @ParameterizedTest
    @MethodSource("invalidIntegerValues")
    void shouldLogTheKeyWhenAnIntegerIsNotAnInteger(String key, String value) {
      Config config =
          ConfigFactory.parseString("webforj.security.bbj.%s = %s".formatted(key, value));

      BBjConfigReader.readConfig(properties, config);

      List<LogRecord> warnings = getWarnings();
      assertEquals(1, warnings.size());
      assertEquals(
          "The configuration key webforj.security.bbj.%s is not an integer and keeps its default"
              .formatted(key),
          new SimpleFormatter().formatMessage(warnings.get(0)));
    }

    @ParameterizedTest
    @MethodSource("outOfRangeIntegerValues")
    void shouldKeepThePreviousIntegerWhenItIsOutOfRange(String key, int value, int min, int max) {
      properties.setAdminPort(11);
      properties.setAdminTimeoutSeconds(12);
      properties.setTokenMinutes(13);
      Config config =
          ConfigFactory.parseString("webforj.security.bbj.%s = %d".formatted(key, value));

      BBjConfigReader.readConfig(properties, config);

      assertEquals(11, properties.getAdminPort());
      assertEquals(12, properties.getAdminTimeoutSeconds());
      assertEquals(13, properties.getTokenMinutes());
    }

    @ParameterizedTest
    @MethodSource("outOfRangeIntegerValues")
    void shouldLogTheKeyWhenAnIntegerIsOutOfRange(String key, int value, int min, int max) {
      Config config =
          ConfigFactory.parseString("webforj.security.bbj.%s = %d".formatted(key, value));

      BBjConfigReader.readConfig(properties, config);

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
      properties.setReturnAfterLogin(false);
      Config config =
          ConfigFactory.parseString("webforj.security.bbj.return-after-login = " + value);

      BBjConfigReader.readConfig(properties, config);

      assertTrue(properties.getReturnAfterLogin());
    }

    @Test
    void shouldLogTheKeyWhenReturnAfterLoginIsNotABoolean() {
      Config config = ConfigFactory.parseString("webforj.security.bbj.return-after-login = 5");

      BBjConfigReader.readConfig(properties, config);

      List<LogRecord> warnings = getWarnings();
      assertEquals(1, warnings.size());
      assertEquals(
          "The configuration key webforj.security.bbj.return-after-login is not a boolean and "
              + "counts as true",
          new SimpleFormatter().formatMessage(warnings.get(0)));
    }

    @ParameterizedTest
    @MethodSource("invalidAdminStringValues")
    void shouldKeepThePreviousAdminStringWhenItIsNotAString(String key, String value) {
      properties.setAdminHost("before.example.com");
      properties.setDefaultSuccessPath("/before-home");
      Config config =
          ConfigFactory.parseString("webforj.security.bbj.%s = %s".formatted(key, value));

      BBjConfigReader.readConfig(properties, config);

      assertEquals("before.example.com", properties.getAdminHost());
      assertEquals("/before-home", properties.getDefaultSuccessPath());
    }

    @ParameterizedTest
    @MethodSource("invalidAdminStringValues")
    void shouldLogTheKeyWhenAnAdminStringIsNotAString(String key, String value) {
      Config config =
          ConfigFactory.parseString("webforj.security.bbj.%s = %s".formatted(key, value));

      BBjConfigReader.readConfig(properties, config);

      List<LogRecord> warnings = getWarnings();
      assertEquals(1, warnings.size());
      assertEquals(
          "The configuration key webforj.security.bbj.%s is not a string and keeps its default"
              .formatted(key),
          new SimpleFormatter().formatMessage(warnings.get(0)));
    }

    private List<LogRecord> getWarnings() {
      return records.stream().filter(record -> record.getLevel() == Level.WARNING).toList();
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

    private static Stream<Arguments> invalidPathValues() {
      return Stream.of(Arguments.of("authentication-path", "[\"/signin\"]"),
          Arguments.of("authentication-path", "{a = 1}"), Arguments.of("authentication-path", "5"),
          Arguments.of("authentication-path", "true"), Arguments.of("deny-path", "[\"/denied\"]"),
          Arguments.of("deny-path", "{a = 1}"), Arguments.of("deny-path", "5"),
          Arguments.of("deny-path", "true"));
    }
  }
}
