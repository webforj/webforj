package com.webforj.i18n;

import com.webforj.App;
import java.io.Serializable;
import java.text.MessageFormat;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.ResourceBundle;

/**
 * Interface for resolving translation keys to localized strings.
 *
 * <p>
 * Implementations of this interface provide the mechanism for looking up translated text based on
 * keys and locales. The framework provides a default implementation
 * {@link BundleTranslationResolver} that uses Java's {@link ResourceBundle} to load translations
 * from properties files.
 * </p>
 *
 * <p>
 * Custom implementations can be registered via {@link App#setTranslationResolver} to support
 * alternative translation sources such as databases, remote services, or custom file formats.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.12
 *
 * @see BundleTranslationResolver
 * @see App#t(String, Object...)
 */
public interface TranslationResolver extends Serializable {

  /**
   * Resolves a translation key to its localized string value.
   *
   * <p>
   * If the key is not found, implementations should return the key itself rather than null or
   * throwing an exception.
   * </p>
   *
   * <p>
   * The args parameter supports {@link MessageFormat} style placeholders. For example, if the
   * resolved text is "Hello {0}, you have {1} messages", calling
   * {@code resolve("greeting", locale, "John", 5)} would return "Hello John, you have 5 messages".
   * </p>
   *
   * @param key the translation key to look up
   * @param locale the locale to use for translation
   * @param args optional arguments for placeholder substitution
   *
   * @return the resolved text, or the key itself if not found
   */
  String resolve(String key, Locale locale, Object... args);

  /**
   * Resolves multiple translation keys to their localized string values.
   *
   * <p>
   * This method is useful for batch lookups, especially when using a database-backed resolver where
   * fetching multiple translations in a single query is more efficient than individual lookups.
   * </p>
   *
   * <p>
   * The default implementation iterates over the keys and calls
   * {@link #resolve(String, Locale, Object...)} for each one. Custom implementations (e.g.,
   * database-backed) can override this to perform a single batch query.
   * </p>
   *
   * @param keys the collection of translation keys to look up, must not be null
   * @param locale the locale to use for translation
   *
   * @return a map of keys to their resolved translations, in encounter order
   * @throws NullPointerException if keys is null
   */
  default Map<String, String> resolve(Collection<String> keys, Locale locale) {
    Objects.requireNonNull(keys, "keys cannot be null");
    Map<String, String> result = new LinkedHashMap<>();
    for (String key : keys) {
      result.putIfAbsent(key, resolve(key, locale));
    }

    return result;
  }

  /**
   * Returns the list of locales supported by this resolver.
   *
   * <p>
   * The first locale in the list is considered the default locale. If no translations are available
   * for a requested locale, implementations may fall back to the default locale.
   * </p>
   *
   * @return list of supported locales, never null but may be empty
   */
  List<Locale> getSupportedLocales();

  /**
   * Returns the default locale for this resolver.
   *
   * <p>
   * By default, this returns the first locale from {@link #getSupportedLocales()}, or
   * {@link App#getLocale()} if no locales are configured.
   * </p>
   *
   * @return the default locale
   */
  default Locale getDefaultLocale() {
    List<Locale> locales = getSupportedLocales();
    if (locales == null || locales.isEmpty()) {
      return App.getLocale();
    }

    return locales.get(0);
  }
}
