package com.webforj.i18n;

import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Optional;
import java.util.Set;
import java.util.StringJoiner;
import java.util.stream.Stream;

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

  /**
   * Parses a locale from its string form or from a BCP 47 language tag.
   *
   * <p>
   * The string form is the one {@link Locale#toString()} writes, with underscores between language,
   * country and variant, and an optional script and extensions after a {@code #}.
   * </p>
   *
   * @param value the locale string (e.g., "de_DE", "zh_TW_#Hant") or the language tag (e.g.,
   *        "de-DE")
   * @return the parsed locale, or an empty optional when the value is not a locale
   */
  public static Optional<Locale> parseLocale(String value) {
    String text = value != null ? value.trim() : "";
    Locale locale = text.contains("_") || text.contains("#") ? parseLocaleString(text)
        : Locale.forLanguageTag(text);
    Locale wellFormed = Locale.forLanguageTag(locale.toLanguageTag());
    if (wellFormed.getLanguage().isEmpty() && wellFormed.getCountry().isEmpty()) {
      return Optional.empty();
    }

    return Optional.of(locale);
  }

  private static Locale parseLocaleString(String text) {
    int hash = text.indexOf('#');
    String base = hash < 0 ? text : text.substring(0, hash).replaceFirst("_$", "");
    String[] parts = base.split("_", 3);
    String country = parts.length > 1 ? parts[1] : "";
    String variant = parts.length > 2 ? parts[2] : "";
    Locale locale = Locale.of(parts[0], country, variant);
    if (hash < 0 || locale.toString().equals(text)) {
      return locale;
    }

    String[] suffix = text.substring(hash + 1).split("_", 2);
    boolean hasScript = !suffix[0].contains("-");
    StringJoiner tag = new StringJoiner("-");
    tag.add(parts[0]);
    if (hasScript) {
      tag.add(suffix[0]);
    }

    Stream.of(country, variant.replace('_', '-')).filter(part -> !part.isEmpty()).forEach(tag::add);
    if (!hasScript || suffix.length > 1) {
      tag.add(suffix[suffix.length - 1]);
    }

    return Locale.forLanguageTag(tag.toString());
  }
}
