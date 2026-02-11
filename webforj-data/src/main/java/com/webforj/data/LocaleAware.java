package com.webforj.data;

import java.util.Locale;

/**
 * Interface for classes that are aware of a locale.
 *
 * @param <T> the type of the implementing class for fluent API support.
 *
 * @since 25.12
 * @author Hyyan Abo Fakher
 */
public interface LocaleAware<T> {

  /**
   * Sets the locale.
   *
   * @param locale the locale.
   * @return the implementing instance.
   */
  T setLocale(Locale locale);

  /**
   * Gets the locale.
   *
   * @return the locale.
   */
  Locale getLocale();
}
