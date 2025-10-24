package com.webforj.i18n.event;

import java.util.EventObject;
import java.util.Locale;

/**
 * Event object that contains data related to locale changes.
 *
 * <p>
 * This event is fired when the application locale changes and is dispatched to all observers
 * implementing {@link com.webforj.i18n.LocaleObserver}.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.10
 */
public class LocaleEvent extends EventObject {

  private final Locale locale;

  /**
   * Constructs a new LocaleEvent.
   *
   * @param source the source of the locale change event
   * @param locale the new locale that was set
   */
  public LocaleEvent(Object source, Locale locale) {
    super(source);
    this.locale = locale;
  }

  /**
   * Gets the new locale that was set.
   *
   * @return the new locale
   */
  public Locale getLocale() {
    return locale;
  }
}
