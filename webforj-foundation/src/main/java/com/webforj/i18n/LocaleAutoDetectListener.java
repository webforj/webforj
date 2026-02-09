package com.webforj.i18n;

import com.typesafe.config.Config;
import com.webforj.App;
import com.webforj.AppLifecycleListener;
import com.webforj.Environment;
import com.webforj.Request;
import com.webforj.annotation.AppListenerPriority;
import java.lang.System.Logger.Level;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

/**
 * Sets the application locale from the browser's preferences on startup.
 *
 * @author Hyyan Abo Fakher
 * @since 25.12
 *
 * @see App#setLocale(Locale)
 */
// Must run after config is available (priority 0 listeners like SpringConfigMerger)
@AppListenerPriority(5)
public class LocaleAutoDetectListener implements AppLifecycleListener {
  private static final System.Logger logger =
      System.getLogger(LocaleAutoDetectListener.class.getName());

  /**
   * {@inheritDoc}
   */
  @Override
  public void onWillRun(App app) {
    if (!isAutoDetectEnabled()) {
      return;
    }

    List<Locale> supportedLocales = getSupportedLocales();

    // Cannot auto-detect without a list to match against
    if (supportedLocales.isEmpty()) {
      return;
    }

    Request request = Request.getCurrent();
    if (request == null) {
      return;
    }

    List<Locale> preferredLocales = request.getPreferredLocales();
    Locale resolved = resolveLocale(preferredLocales, supportedLocales);
    App.setLocale(resolved);

    logger.log(Level.DEBUG, "Auto-detected locale: {0}", resolved);
  }

  /**
   * Resolves the best matching locale from browser preferences against supported locales.
   *
   * <p>
   * Matching strategy (in priority order):
   * <ol>
   * <li>Exact match - preferred locale equals a supported locale</li>
   * <li>Language match - preferred language matches a supported locale's language</li>
   * <li>Fallback - first supported locale</li>
   * </ol>
   * </p>
   *
   * @param preferredLocales the browser's preferred locales
   * @param supportedLocales the application's supported locales
   * @return the best matching locale, or the app locale if no supported locales
   */
  @SuppressWarnings("java:S3776") // Cognitive Complexity of methods should not be too high
  private Locale resolveLocale(List<Locale> preferredLocales, List<Locale> supportedLocales) {
    if (supportedLocales == null || supportedLocales.isEmpty()) {
      return App.getLocale();
    }

    // Build lookup structures from supported locales
    Set<Locale> exactLookup = new HashSet<>();
    Map<String, Locale> languageLookup = new HashMap<>();
    Locale fallback = null;

    for (Locale locale : supportedLocales) {
      if (locale == null) {
        continue;
      }

      if (fallback == null) {
        fallback = locale;
      }

      exactLookup.add(locale);
      String lang = locale.getLanguage();
      if (!lang.isEmpty()) {
        languageLookup.putIfAbsent(lang, locale);
      }
    }

    if (fallback == null) {
      return App.getLocale();
    }

    if (preferredLocales == null || preferredLocales.isEmpty()) {
      return fallback;
    }

    // check exact match first, then language match
    for (Locale preferred : preferredLocales) {
      if (preferred == null) {
        continue;
      }
      if (exactLookup.contains(preferred)) {
        return preferred;
      }
      String lang = preferred.getLanguage();
      if (!lang.isEmpty() && languageLookup.containsKey(lang)) {
        return languageLookup.get(lang);
      }
    }

    return fallback;
  }

  private boolean isAutoDetectEnabled() {
    Environment env = Environment.getCurrent();
    if (env != null) {
      Config config = env.getConfig();
      String path = "webforj.i18n.auto-detect";
      if (config != null && config.hasPath(path) && !config.getIsNull(path)) {
        return config.getBoolean(path);
      }
    }

    return false;
  }

  private List<Locale> getSupportedLocales() {
    Environment env = Environment.getCurrent();
    if (env != null) {
      Config config = env.getConfig();
      String path = "webforj.i18n.supported-locales";
      if (config != null && config.hasPath(path) && !config.getIsNull(path)) {
        return LocaleUtils.parseLocaleTags(config.getStringList(path));
      }
    }

    return List.of();
  }
}
