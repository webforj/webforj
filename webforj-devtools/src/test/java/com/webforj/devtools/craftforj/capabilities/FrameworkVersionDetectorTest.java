package com.webforj.devtools.craftforj.capabilities;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class FrameworkVersionDetectorTest {

  @Nested
  @DisplayName("isAtLeast")
  class IsAtLeast {

    @Test
    @DisplayName("Should meet a requirement at the same major and minor")
    void shouldMeetSameVersion() {
      assertTrue(new FrameworkVersionDetector("26.02").isAtLeast(26, 2));
    }

    @Test
    @DisplayName("Should meet a requirement below the detected version")
    void shouldMeetLowerRequirement() {
      assertTrue(new FrameworkVersionDetector("26.03").isAtLeast(26, 2));
    }

    @Test
    @DisplayName("Should not meet a requirement above the detected version")
    void shouldNotMeetHigherRequirement() {
      assertFalse(new FrameworkVersionDetector("26.01").isAtLeast(26, 2));
    }

    @Test
    @DisplayName("Should strip a qualifier before comparing")
    void shouldStripQualifier() {
      assertTrue(new FrameworkVersionDetector("26.02-SNAPSHOT").isAtLeast(26, 2));
    }

    @ParameterizedTest
    @DisplayName("Should not meet any requirement for an unparseable version")
    @ValueSource(strings = {"", "garbage", "26"})
    void shouldNotMeetForUnparseable(String version) {
      assertFalse(new FrameworkVersionDetector(version).isAtLeast(26, 2));
    }

    @Test
    @DisplayName("Should not meet any requirement for a null version")
    void shouldNotMeetForNull() {
      assertFalse(new FrameworkVersionDetector((String) null).isAtLeast(26, 2));
    }
  }

  @Test
  @DisplayName("Should expose the given version string")
  void shouldExposeVersion() {
    assertEquals("26.02-SNAPSHOT", new FrameworkVersionDetector("26.02-SNAPSHOT").getVersion());
  }

  @Test
  @DisplayName("Should resolve from the classpath without throwing")
  void shouldResolveFromClasspath() {
    assertDoesNotThrow(() -> new FrameworkVersionDetector().isAtLeast(26, 2));
  }
}
