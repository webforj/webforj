package com.webforj.i18n;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.stream.Stream;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class LocaleUtilsTest {

  @Nested
  class ParseLocaleTags {

    static Stream<Arguments> parsingCases() {
      // @formatter:off
      return Stream.of(
          // Valid locales
          Arguments.of(
              Arrays.asList("en", "fr", "de"),
              Arrays.asList(Locale.ENGLISH, Locale.FRENCH, Locale.GERMAN)),
          // Skips empty and blank tags
          Arguments.of(
              Arrays.asList("en", "", "  ", "fr"),
              Arrays.asList(Locale.ENGLISH, Locale.FRENCH)),
          // Trims whitespace
          Arguments.of(
              Arrays.asList(" en ", " fr "),
              Arrays.asList(Locale.ENGLISH, Locale.FRENCH)),
          // Deduplicates while preserving order
          Arguments.of(
              Arrays.asList("en", "fr", "en", "de", "fr"),
              Arrays.asList(Locale.ENGLISH, Locale.FRENCH, Locale.GERMAN)),
          // Empty list
          Arguments.of(
              Collections.emptyList(),
              Collections.emptyList()),
          // Null values in list are skipped
          Arguments.of(
              Arrays.asList("en", null, "fr"),
              Arrays.asList(Locale.ENGLISH, Locale.FRENCH)),
          // Complex locale tags
          Arguments.of(
              Arrays.asList("en-US", "fr-CA", "de-DE"),
              Arrays.asList(Locale.US, Locale.CANADA_FRENCH, Locale.GERMANY))
      );
      // @formatter:on
    }

    @ParameterizedTest
    @MethodSource("parsingCases")
    void shouldParseLocaleTags(List<String> tags, List<Locale> expected) {
      List<Locale> result = LocaleUtils.parseLocaleTags(tags);
      assertEquals(expected, result);
    }

    @Test
    void shouldReturnEmptyListForNull() {
      List<Locale> result = LocaleUtils.parseLocaleTags(null);
      assertTrue(result.isEmpty());
    }

    @Test
    void shouldSkipTagsThatProduceEmptyLanguage() {
      List<Locale> result =
          LocaleUtils.parseLocaleTags(Arrays.asList("en", "123", "!!!", "@#$", "456-US", "fr"));

      assertEquals(Arrays.asList(Locale.ENGLISH, Locale.FRENCH), result);
    }

    @Test
    void shouldSkipUndeterminedLocale() {
      List<Locale> result = LocaleUtils.parseLocaleTags(Arrays.asList("und", "en"));

      assertEquals(1, result.size());
      assertEquals(Locale.ENGLISH, result.get(0));
    }
  }
}
