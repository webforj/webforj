package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.typesafe.config.ConfigFactory;
import com.webforj.bbj.security.fixture.CapturedLog;
import java.lang.System.Logger;
import java.util.List;
import java.util.Optional;
import java.util.logging.Level;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class BBjConfigReaderTest {
  private static final Logger LOGGER = System.getLogger(BBjConfigReaderTest.class.getName());
  private CapturedLog log;

  @BeforeEach
  void setUp() {
    log = new CapturedLog(BBjConfigReaderTest.class.getName());
  }

  @AfterEach
  void tearDown() {
    log.close();
  }

  private static BBjConfigReader createReader(String hocon) {
    return new BBjConfigReader(ConfigFactory.parseString(hocon), LOGGER);
  }

  @ParameterizedTest
  @ValueSource(strings = {"1", "5", "10"})
  void shouldReadAnIntegerWithinTheBoundsInclusive(String value) {
    BBjConfigReader reader = createReader("a = " + value);

    assertEquals(Integer.parseInt(value), reader.readInt("a", 3, 1, 10));
    assertTrue(log.getMessages(Level.WARNING).isEmpty());
  }

  @ParameterizedTest
  @ValueSource(strings = {"0", "11", "-1", "1.5", "\"x\"", "true", "[1]", "{b = 1}"})
  void shouldUseTheDefaultAndWarnOnceForAnInvalidInteger(String value) {
    BBjConfigReader reader = createReader("a = " + value);

    assertEquals(3, reader.readInt("a", 3, 1, 10));
    assertEquals(List.of("Invalid value for a, expected a whole number from 1 to 10, using 3"),
        log.getMessages(Level.WARNING));
  }

  @Test
  void shouldUseTheDefaultAndLogDebugForMissingOrNullInteger() {
    BBjConfigReader reader = createReader("b = null");

    assertEquals(3, reader.readInt("a", 3, 1, 10));
    assertEquals(3, reader.readInt("b", 3, 1, 10));
    assertEquals(List.of("a is not set, using 3", "b is not set, using 3"),
        log.getMessages(Level.FINE));
    assertTrue(log.getMessages(Level.WARNING).isEmpty());
  }

  @Test
  void shouldReadAnIntegerWrittenAsString() {
    assertEquals(7, createReader("a = \"7\"").readInt("a", 3, 1, 10));
  }

  @Test
  void shouldClampAnIntegerAboveTheMaximumAndWarnOnce() {
    BBjConfigReader reader = createReader("a = 11");

    assertEquals(10, reader.readClampedInt("a", 3, 1, 10));
    assertEquals(List.of("Value of a is above 10, using 10"), log.getMessages(Level.WARNING));
  }

  @Test
  void shouldKeepAnIntegerAtTheMaximumWithoutWarning() {
    assertEquals(10, createReader("a = 10").readClampedInt("a", 3, 1, 10));
    assertTrue(log.getMessages(Level.WARNING).isEmpty());
  }

  @ParameterizedTest
  @ValueSource(strings = {"0", "-4", "\"x\"", "1.5"})
  void shouldUseTheDefaultWhenClampedIntegerIsBelowTheMinimumOrNotNumber(String value) {
    BBjConfigReader reader = createReader("a = " + value);

    assertEquals(3, reader.readClampedInt("a", 3, 1, 10));
    assertEquals(List.of("Invalid value for a, expected a whole number from 1 to 10, using 3"),
        log.getMessages(Level.WARNING));
  }

  @Test
  void shouldUseTheDefaultForMissingClampedInteger() {
    assertEquals(3, createReader("").readClampedInt("a", 3, 1, 10));
    assertEquals(List.of("a is not set, using 3"), log.getMessages(Level.FINE));
  }

  @Test
  void shouldThrowForAnInvalidRequiredIntegerWithoutTheValue() {
    BBjConfigReader reader = createReader("a = \"secretValue\"");

    IllegalArgumentException exception =
        assertThrows(IllegalArgumentException.class, () -> reader.requireInt("a", 3, 1, 10));

    assertEquals("Invalid value for a, expected a whole number from 1 to 10",
        exception.getMessage());
    assertTrue(log.getAllMessages().stream().noneMatch(text -> text.contains("secretValue")));
  }

  @Test
  void shouldReadRequiredIntegerAndUseTheDefaultWhenMissing() {
    assertEquals(4, createReader("a = 4").requireInt("a", 3, 1, 10));
    assertEquals(3, createReader("").requireInt("a", 3, 1, 10));
  }

  @Test
  void shouldReadBooleanAndUseTheFallbacks() {
    assertTrue(createReader("a = true").readBoolean("a", false, false));
    assertTrue(createReader("").readBoolean("a", true, false));
    assertEquals(List.of("a is not set, using true"), log.getMessages(Level.FINE));
    assertFalse(createReader("a = maybe").readBoolean("a", true, false));
    assertEquals(List.of("Invalid value for a, expected true or false, using false"),
        log.getMessages(Level.WARNING));
  }

  @Test
  void shouldThrowForAnInvalidRequiredBoolean() {
    BBjConfigReader reader = createReader("a = maybe");

    assertThrows(IllegalArgumentException.class, () -> reader.requireBoolean("a", false));
    assertFalse(createReader("").requireBoolean("a", false));
    assertTrue(createReader("a = true").requireBoolean("a", false));
  }

  @Test
  void shouldReadStringThatPassesTheValidator() {
    BBjConfigReader reader = createReader("a = \"/ok\"");

    assertEquals(Optional.of("/ok"), reader.readString("a", "none", value -> true, "a text"));
  }

  @Test
  void shouldWarnAndReturnNothingForStringThatFailsTheValidator() {
    BBjConfigReader reader = createReader("a = \"bad\"");

    assertEquals(Optional.empty(), reader.readString("a", "none", value -> false, "a text"));
    assertEquals(List.of("Invalid value for a, expected a text, using none"),
        log.getMessages(Level.WARNING));
  }

  @Test
  void shouldReturnNothingForMissingStringAndLogDebug() {
    assertEquals(Optional.empty(), createReader("").readString("a", "none", value -> true, "t"));
    assertEquals(List.of("a is not set, using none"), log.getMessages(Level.FINE));
  }

  @Test
  void shouldWarnForStringThatIsNotString() {
    BBjConfigReader reader = createReader("a = [1]");

    assertEquals(Optional.empty(), reader.readString("a", "none", value -> true, "a text"));
    assertEquals(1, log.getMessages(Level.WARNING).size());
  }

  @Test
  void shouldThrowForRequiredStringThatFailsTheValidator() {
    BBjConfigReader reader = createReader("a = \"bad\"");

    assertThrows(IllegalArgumentException.class,
        () -> reader.requireString("a", "none", value -> false, "a text"));
    assertEquals(Optional.of("bad"), reader.requireString("a", "none", value -> true, "a text"));
    assertEquals(Optional.empty(),
        createReader("").requireString("a", "none", value -> true, "a text"));
  }

  @Test
  void shouldTreatAnUnresolvedSubstitutionAsAnInvalidValue() {
    BBjConfigReader reader = createReader("a = ${nowhere.nothing}");

    assertEquals(3, reader.readInt("a", 3, 1, 10));
    assertThrows(IllegalArgumentException.class, () -> reader.requireInt("a", 3, 1, 10));
  }
}
