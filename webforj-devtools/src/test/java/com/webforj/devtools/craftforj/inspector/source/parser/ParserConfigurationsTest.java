package com.webforj.devtools.craftforj.inspector.source.parser;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.github.javaparser.ParserConfiguration.LanguageLevel;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class ParserConfigurationsTest {

  @Test
  @DisplayName("Should resolve a level the bundled JavaParser knows for the running JVM")
  void shouldResolveLevelForRunningJvm() {
    LanguageLevel level = ParserConfigurations.getLanguageLevel();

    assertNotNull(level);
    assertTrue(level.name().startsWith("JAVA_"));
  }

  @Test
  @DisplayName("Should resolve the exact level when JavaParser supports the feature version")
  void shouldResolveExactSupportedLevel() {
    assertEquals(LanguageLevel.JAVA_21, ParserConfigurations.resolveLanguageLevel(21));
  }

  @Test
  @DisplayName("Should clamp a future feature version to the newest supported level")
  void shouldClampFutureFeatureVersion() {
    LanguageLevel level = ParserConfigurations.resolveLanguageLevel(99);

    assertNotNull(level);
    int resolved = Integer.parseInt(level.name().substring("JAVA_".length()));
    assertTrue(resolved >= 21);
  }

  @Test
  @DisplayName("Should never resolve below the webforJ minimum of Java 21")
  void shouldNeverResolveBelowMinimum() {
    assertEquals(LanguageLevel.JAVA_21, ParserConfigurations.resolveLanguageLevel(17));
  }

  @Test
  @DisplayName("Should hand out configurations at the resolved level")
  void shouldHandOutConfigurationsAtResolvedLevel() {
    assertEquals(ParserConfigurations.getLanguageLevel(),
        ParserConfigurations.create().getLanguageLevel());
  }
}
