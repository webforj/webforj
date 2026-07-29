package com.webforj.devtools.craftforj.capabilities;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.stream.Stream;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;

class VersionDetectorTest {

  @Nested
  @DisplayName("parseVersion")
  class ParseVersion {

    @ParameterizedTest(name = "\"{0}\" -> [{1}, {2}]")
    @DisplayName("Should parse valid version strings")
    @CsvSource({"25.12, 25, 12", "25.12-SNAPSHOT, 25, 12", "25.12-RC1, 25, 12", "25.12.1, 25, 12",})
    void shouldParseValidVersions(String input, int expectedMajor, int expectedMinor) {
      int[] result = VersionDetector.parseVersion(input);
      assertArrayEquals(new int[] {expectedMajor, expectedMinor}, result);
    }

    @ParameterizedTest(name = "\"{0}\" -> empty")
    @DisplayName("Should return empty array for unparseable input")
    @NullAndEmptySource
    @ValueSource(strings = {"25", "abc.def"})
    void shouldReturnEmptyForUnparseableInput(String input) {
      int[] result = VersionDetector.parseVersion(input);
      assertEquals(0, result.length);
    }
  }

  @Nested
  @DisplayName("isAtLeast")
  class IsAtLeast {

    @ParameterizedTest(name = "\"{0}\" >= 25.12")
    @DisplayName("Should return true when version meets requirement")
    @ValueSource(strings = {"25.12-SNAPSHOT", "26.0", "25.13"})
    void shouldReturnTrueWhenMet(String version) {
      assertTrue(new VersionDetector(version).isAtLeast(25, 12));
    }

    @ParameterizedTest(name = "\"{0}\" < 25.12")
    @DisplayName("Should return false when version does not meet requirement")
    @MethodSource("belowRequirement")
    void shouldReturnFalseWhenNotMet(String version) {
      assertFalse(new VersionDetector(version).isAtLeast(25, 12));
    }

    static Stream<String> belowRequirement() {
      return Stream.of("24.99", "25.11", null, "invalid");
    }
  }

  @Nested
  @DisplayName("getVersion")
  class GetVersion {

    @Test
    @DisplayName("Should return the raw version string")
    void shouldReturnRawVersion() {
      VersionDetector detector = new VersionDetector("25.12-SNAPSHOT");
      assertEquals("25.12-SNAPSHOT", detector.getVersion());
    }

    @Test
    @DisplayName("Should return null when constructed with null")
    void shouldReturnNullWhenConstructedWithNull() {
      VersionDetector detector = new VersionDetector(null);
      assertNull(detector.getVersion());
    }
  }
}
