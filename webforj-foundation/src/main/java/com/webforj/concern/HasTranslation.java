package com.webforj.concern;

import com.webforj.App;
import com.webforj.i18n.TranslationResolver;
import java.util.Locale;

/**
 * An interface that provides translation capabilities to components.
 *
 * <p>
 * Components implementing this interface gain access to the {@link #t(String, Object...)} method
 * for resolving translation keys to localized strings.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.12
 *
 * @see App#t(String, Object...)
 * @see TranslationResolver
 */
public interface HasTranslation {

  /**
   * Resolves a translation key to its localized string value.
   *
   * <p>
   * This is a convenience method for translating text keys. It delegates to
   * {@link App#t(String, Object...)} using the application's current locale.
   * </p>
   *
   * @param key the translation key to look up
   * @param args optional arguments for placeholder substitution (MessageFormat style)
   *
   * @return the resolved text, or the key itself if not found
   */
  default String t(String key, Object... args) {
    return App.t(key, args);
  }

  /**
   * Resolves a translation key to its localized string value for a specific locale.
   *
   * @param locale the locale to use for translation
   * @param key the translation key to look up
   * @param args optional arguments for placeholder substitution
   *
   * @return the resolved text, or the key itself if not found
   */
  default String t(Locale locale, String key, Object... args) {
    return App.t(locale, key, args);
  }
}
