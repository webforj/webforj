package com.webforj.i18n;

import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;

/**
 * Utility class for parsing and working with locales.
 *
 * @author Hyyan Abo Fakher
 * @since 25.12
 */
public final class LocaleUtils {

  private LocaleUtils() {
    // Utility class
  }

  /**
   * Parses a collection of BCP 47 language tags into a list of locales.
   *
   * @param tags the collection of BCP 47 language tags (e.g., "en", "en-US", "fr-CA")
   * @return an unmodifiable list of valid, unique locales in the order they were first encountered
   */
  public static List<Locale> parseLocaleTags(Collection<String> tags) {
    if (tags == null || tags.isEmpty()) {
      return List.of();
    }

    Set<Locale> seen = new LinkedHashSet<>();
    for (String tag : tags) {
      String trimmed = tag != null ? tag.trim() : "";
      if (!trimmed.isEmpty()) {
        Locale locale = Locale.forLanguageTag(trimmed);
        // Only add valid locales (invalid tags produce empty language)
        if (!locale.getLanguage().isEmpty()) {
          seen.add(locale);
        }
      }
    }

    return List.copyOf(seen);
  }
}
