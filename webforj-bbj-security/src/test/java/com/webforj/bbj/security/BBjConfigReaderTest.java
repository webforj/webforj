package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;
import java.util.ArrayList;
import java.util.List;
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

    private List<LogRecord> getWarnings() {
      return records.stream().filter(record -> record.getLevel() == Level.WARNING).toList();
    }

    private static Stream<Arguments> invalidBooleanKeys() {
      return Stream.of(Arguments.of("enabled"), Arguments.of("secure-by-default"));
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
