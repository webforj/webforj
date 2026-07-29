package com.webforj.devtools.craftforj.inspector.action;

import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.action.CraftforjActionHandler;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandlerRegistry;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.util.Enumeration;
import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

/**
 * Action handler that returns localized property description translations.
 *
 * <p>
 * This action builds a flat translation catalog for the requested locale by loading the core
 * inspector description bundle and merging in every distinct translation bundle contributed by
 * registered {@link FeatureHandler} instances. The client merges the returned catalog into its own
 * i18n registry to render property description tooltips.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class GetTranslationsAction
    implements CraftforjActionHandler<GetTranslationsAction.Response> {

  /**
   * The action name for this handler.
   */
  public static final String ACTION = "inspector.getTranslations";

  private static final String CORE_BUNDLE =
      "com.webforj.devtools.craftforj.inspector.i18n.craftforj-inspector";
  private static final String DEFAULT_LOCALE = "en";
  private static final Logger LOGGER = System.getLogger(GetTranslationsAction.class.getName());

  private final FeatureHandlerRegistry registry;
  private final String coreBundle;

  /**
   * Creates a new GetTranslationsAction with a default feature handler registry.
   */
  public GetTranslationsAction() {
    this(new FeatureHandlerRegistry());
  }

  /**
   * Creates a new GetTranslationsAction with the given feature handler registry.
   *
   * @param registry the feature handler registry to pull contributed bundles from
   */
  GetTranslationsAction(FeatureHandlerRegistry registry) {
    this(registry, CORE_BUNDLE);
  }

  /**
   * Creates a new GetTranslationsAction with the given feature handler registry and core bundle
   * base name. Used by tests to point at a fixture bundle instead of the real descriptions bundle.
   *
   * @param registry the feature handler registry to pull contributed bundles from
   * @param coreBundle the base name of the core translation bundle
   */
  GetTranslationsAction(FeatureHandlerRegistry registry, String coreBundle) {
    this.registry = registry;
    this.coreBundle = coreBundle;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getAction() {
    return ACTION;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Response handle(JsonObject params) {
    String locale = extractLocale(params);
    Locale target = Locale.forLanguageTag(locale);

    Map<String, String> translations = new LinkedHashMap<>();
    mergeBundle(coreBundle, getClass().getClassLoader(), target, translations, Level.DEBUG);

    for (Map.Entry<String, ClassLoader> contributed : collectContributedBundles().entrySet()) {
      mergeBundle(contributed.getKey(), contributed.getValue(), target, translations,
          Level.WARNING);
    }

    return new Response(locale, translations);
  }

  /**
   * Extracts the requested locale from the request params, defaulting to "en" when missing or
   * blank.
   *
   * @param params the request params
   * @return the requested BCP47 locale tag
   */
  private String extractLocale(JsonObject params) {
    if (params != null && params.has("locale") && !params.get("locale").isJsonNull()) {
      String value = params.get("locale").getAsString();
      if (value != null && !value.isBlank()) {
        return value;
      }
    }

    return DEFAULT_LOCALE;
  }

  /**
   * Collects the distinct, non-null translation bundle names contributed by registered feature
   * handlers, in registration order. Each bundle is resolved with its contributing handler's
   * classloader, so bundles shipped in application jars stay loadable regardless of classloader
   * hierarchy.
   *
   * @return the contributed bundle base names mapped to their handler's classloader
   */
  private Map<String, ClassLoader> collectContributedBundles() {
    Map<String, ClassLoader> bundles = new LinkedHashMap<>();
    for (FeatureHandler handler : registry.getHandlers()) {
      String bundle = handler.getTranslationBundle();
      if (bundle != null && !bundle.isBlank()) {
        bundles.putIfAbsent(bundle, handler.getClass().getClassLoader());
      }
    }

    return bundles;
  }

  /**
   * Loads a resource bundle for the given locale and merges its entries into the target map,
   * overwriting any existing keys. A missing bundle is skipped and logged at the given level, since
   * a description catalog is optional. Uses the no-fallback control so a request for the base
   * locale never resolves to the JVM default locale's bundle.
   *
   * @param bundleName the resource bundle base name
   * @param loader the classloader to resolve the bundle with
   * @param locale the target locale
   * @param target the map to merge translations into
   * @param missingLevel the log level to use when the bundle cannot be loaded
   */
  private void mergeBundle(String bundleName, ClassLoader loader, Locale locale,
      Map<String, String> target, Level missingLevel) {
    ResourceBundle bundle;
    try {
      bundle = ResourceBundle.getBundle(bundleName, locale, loader,
          ResourceBundle.Control.getNoFallbackControl(ResourceBundle.Control.FORMAT_DEFAULT));
    } catch (MissingResourceException e) {
      if (LOGGER.isLoggable(missingLevel)) {
        LOGGER.log(missingLevel, "No translation bundle found: {0}", bundleName);
      }

      return;
    }

    Enumeration<String> keys = bundle.getKeys();
    while (keys.hasMoreElements()) {
      String key = keys.nextElement();
      target.put(key, bundle.getString(key));
    }
  }

  /**
   * Response containing the localized translation catalog.
   */
  public static class Response {

    private final String locale;
    private final Map<String, String> translations;

    Response(String locale, Map<String, String> translations) {
      this.locale = locale;
      this.translations = translations;
    }

    /**
     * Gets the resolved locale.
     *
     * @return the locale tag
     */
    public String getLocale() {
      return locale;
    }

    /**
     * Gets the flat translation catalog.
     *
     * @return the translations, keyed by translation key
     */
    public Map<String, String> getTranslations() {
      return translations;
    }
  }
}
