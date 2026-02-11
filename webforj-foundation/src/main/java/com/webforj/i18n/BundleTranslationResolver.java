package com.webforj.i18n;

import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.text.MessageFormat;
import java.util.List;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

/**
 * Default implementation of {@link TranslationResolver} that uses Java's {@link ResourceBundle} to
 * load translations from properties files.
 *
 * <p>
 * This resolver looks for translation files in the classpath following the standard
 * {@link ResourceBundle} naming convention:
 * </p>
 *
 * <ul>
 * <li>{@code messages.properties} - default/fallback translations</li>
 * <li>{@code messages_fr.properties} - French translations</li>
 * <li>{@code messages_de_DE.properties} - German (Germany) translations</li>
 * </ul>
 *
 * <p>
 * The resolver supports {@link MessageFormat} style placeholders in translation values. For
 * example:
 * </p>
 *
 * <pre>
 * # messages.properties
 * greeting=Hello {0}, you have {1} new messages
 *
 * // Usage
 * resolver.resolve("greeting", Locale.ENGLISH, "John", 5);
 * // Returns: "Hello John, you have 5 new messages"
 * </pre>
 *
 * @author Hyyan Abo Fakher
 * @since 25.12
 *
 * @see TranslationResolver
 * @see ResourceBundle
 */
public class BundleTranslationResolver implements TranslationResolver {
  /** Default bundle name used when no custom name is specified. */
  public static final String DEFAULT_BUNDLE_NAME = "messages";

  private final String bundleName;
  private final List<Locale> supportedLocales;
  private transient ClassLoader classLoader;
  private static final Logger logger = System.getLogger(BundleTranslationResolver.class.getName());

  /**
   * Creates a new resolver with the default bundle name "messages".
   *
   * @param supportedLocales the list of supported locales
   */
  public BundleTranslationResolver(List<Locale> supportedLocales) {
    this(DEFAULT_BUNDLE_NAME, supportedLocales);
  }

  /**
   * Creates a new resolver with the default bundle name and custom class loader.
   *
   * @param supportedLocales the list of supported locales
   * @param classLoader the class loader to use for loading the bundle
   */
  public BundleTranslationResolver(List<Locale> supportedLocales, ClassLoader classLoader) {
    this(DEFAULT_BUNDLE_NAME, supportedLocales, classLoader);
  }

  /**
   * Creates a new resolver with a custom bundle name.
   *
   * @param bundleName the base name of the resource bundle (e.g., "messages" or
   *        "com.myapp.translations")
   * @param supportedLocales the list of supported locales
   */
  public BundleTranslationResolver(String bundleName, List<Locale> supportedLocales) {
    this(bundleName, supportedLocales, BundleTranslationResolver.class.getClassLoader());
  }

  /**
   * Creates a new resolver with a custom bundle name and class loader.
   *
   * <p>
   * Use this constructor when resources are loaded from a different class loader, such as in
   * Spring, OSGi, or other complex deployment scenarios.
   * </p>
   *
   * @param bundleName the base name of the resource bundle
   * @param supportedLocales the list of supported locales
   * @param classLoader the class loader to use for loading the bundle
   */
  public BundleTranslationResolver(String bundleName, List<Locale> supportedLocales,
      ClassLoader classLoader) {
    this.bundleName = bundleName != null ? bundleName : DEFAULT_BUNDLE_NAME;
    this.supportedLocales = supportedLocales != null ? List.copyOf(supportedLocales) : List.of();
    this.classLoader =
        classLoader != null ? classLoader : BundleTranslationResolver.class.getClassLoader();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String resolve(String key, Locale locale, Object... args) {
    if (key == null || key.isEmpty()) {
      return "";
    }

    Locale resolvedLocale = locale != null ? locale : getDefaultLocale();
    Object[] resolvedArgs = args != null ? args : new Object[0];
    String value = key;

    ResourceBundle bundle = getBundle(resolvedLocale);
    if (bundle != null) {
      try {
        value = bundle.getString(key);
      } catch (MissingResourceException e) {
        if (logger.isLoggable(Level.WARNING)) {
          logger.log(Level.WARNING, "Missing translation key ''{0}'' for locale ''{1}''", key,
              resolvedLocale);
        }
      }
    }

    if (resolvedArgs.length > 0) {
      try {
        value = new MessageFormat(value, resolvedLocale).format(resolvedArgs);
      } catch (IllegalArgumentException e) {
        if (logger.isLoggable(Level.WARNING)) {
          logger.log(Level.WARNING,
              "Failed to format translation value ''{0}'' for key ''{1}'': {2}", value, key,
              e.getMessage());
        }
      }
    }

    return value;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<Locale> getSupportedLocales() {
    return supportedLocales;
  }

  /**
   * Returns the bundle name used by this resolver.
   *
   * @return the bundle name
   */
  public String getBundleName() {
    return bundleName;
  }

  /**
   * Loads the resource bundle for the specified locale.
   *
   * @param locale the locale to load the bundle for
   * @return the resource bundle, or null if not found
   */
  protected ResourceBundle getBundle(Locale locale) {
    try {
      ClassLoader loader = classLoader;
      if (loader == null) {
        loader = Thread.currentThread().getContextClassLoader();
      }

      return ResourceBundle.getBundle(bundleName, locale, loader);
    } catch (MissingResourceException e) {
      return null;
    }
  }
}
