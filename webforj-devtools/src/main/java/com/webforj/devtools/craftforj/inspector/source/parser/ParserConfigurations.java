package com.webforj.devtools.craftforj.inspector.source.parser;

import com.github.javaparser.ParserConfiguration;
import com.github.javaparser.ParserConfiguration.LanguageLevel;

/**
 * Single source of the JavaParser configuration used across craftforJ.
 *
 * <p>
 * The language level follows the JVM the application runs on, clamped to the newest level the
 * bundled JavaParser knows. Sources are always parsed at the level closest to what the project
 * actually compiles with, and a JavaParser upgrade lifts every parser in craftforJ at once instead
 * of chasing scattered hardcoded levels.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
final class ParserConfigurations {

  private static final int MINIMUM_FEATURE = 21;
  private static final LanguageLevel LEVEL = resolveLanguageLevel(Runtime.version().feature());

  private ParserConfigurations() {}

  /**
   * Creates a parser configuration at the resolved language level.
   *
   * @return a fresh configuration
   */
  static ParserConfiguration create() {
    return new ParserConfiguration().setLanguageLevel(LEVEL);
  }

  /**
   * Gets the language level every craftforJ parser runs at.
   *
   * @return the resolved language level
   */
  static LanguageLevel getLanguageLevel() {
    return LEVEL;
  }

  /**
   * Resolves the closest language level the bundled JavaParser supports for a JVM feature version.
   *
   * <p>
   * Walks down from the running feature version until a matching constant exists, so a JVM newer
   * than the bundled JavaParser degrades to the newest supported level instead of failing. The walk
   * never goes below Java 21, the minimum version webforJ supports.
   * </p>
   *
   * @param feature the JVM feature version, for example 25
   * @return the resolved language level
   */
  static LanguageLevel resolveLanguageLevel(int feature) {
    for (int version = feature; version >= MINIMUM_FEATURE; version--) {
      try {
        return LanguageLevel.valueOf("JAVA_" + version);
      } catch (IllegalArgumentException e) {
        // The bundled JavaParser predates this Java version, try the one below
      }
    }

    return LanguageLevel.JAVA_21;
  }
}
