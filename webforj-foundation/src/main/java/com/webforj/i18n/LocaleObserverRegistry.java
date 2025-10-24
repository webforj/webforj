package com.webforj.i18n;

import com.webforj.environment.ObjectTable;
import com.webforj.i18n.event.LocaleEvent;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.util.Locale;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * A registry for managing {@link LocaleObserver} instances.
 *
 * <p>
 * This class maintains a per-app registry of all components that want to be notified of locale
 * changes. Components are automatically registered when they implement {@link LocaleObserver} and
 * are created, and automatically unregistered when they are destroyed.
 * </p>
 *
 * <p>
 * When the application locale changes via {@link com.webforj.App#setLocale}, this registry is used
 * to notify all registered observers by firing {@link LocaleEvent} events to them.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.10
 */
public final class LocaleObserverRegistry {
  private static final Logger logger = System.getLogger(LocaleObserverRegistry.class.getName());
  private final Set<LocaleObserver> observers = ConcurrentHashMap.newKeySet();

  /**
   * Private constructor to prevent direct instantiation.
   */
  private LocaleObserverRegistry() {}

  /**
   * Gets the current registry instance for this app.
   *
   * @return the current LocaleObserverRegistry instance
   */
  public static LocaleObserverRegistry getCurrent() {
    String key = LocaleObserverRegistry.class.getName();
    if (ObjectTable.contains(key)) {
      return (LocaleObserverRegistry) ObjectTable.get(key);
    }

    LocaleObserverRegistry registry = new LocaleObserverRegistry();
    ObjectTable.put(key, registry);

    return registry;
  }

  /**
   * Registers a locale observer to receive locale change notifications.
   *
   * <p>
   * This method is typically called automatically by the component lifecycle when a component
   * implementing {@link LocaleObserver} is created. Manual registration is not normally required.
   * </p>
   *
   * @param observer the observer to register
   */
  public void register(LocaleObserver observer) {
    Objects.requireNonNull(observer, "Observer cannot be null");

    boolean added = observers.add(observer);
    if (added && logger.isLoggable(Level.TRACE)) {
      logger.log(Level.TRACE,
          String.format("Registered locale observer: %s", observer.getClass().getName()));
    }
  }

  /**
   * Unregisters a locale observer so it no longer receives locale change notifications.
   *
   * <p>
   * This method is typically called automatically by the component lifecycle when a component is
   * destroyed. Manual unregistration is not normally required.
   * </p>
   *
   * @param observer the observer to unregister
   */
  public void unregister(LocaleObserver observer) {
    if (observer == null) {
      return;
    }

    boolean removed = observers.remove(observer);
    if (removed && logger.isLoggable(Level.TRACE)) {
      logger.log(Level.TRACE,
          String.format("Unregistered locale observer: %s", observer.getClass().getName()));
    }
  }

  /**
   * Notifies all registered observers of a locale change.
   *
   * <p>
   * This method is called by {@link com.webforj.App#setLocale} to propagate locale changes to all
   * registered components. For each observer, a {@link LocaleEvent} is created and the observer's
   * {@link LocaleObserver#onLocaleChange} method is invoked.
   * </p>
   *
   * @param locale the new locale
   */
  public void fireLocaleChange(Locale locale) {
    Objects.requireNonNull(locale, "Locale cannot be null");

    if (logger.isLoggable(Level.DEBUG)) {
      logger.log(Level.DEBUG,
          String.format("Firing locale change to %d observers: %s", observers.size(), locale));
    }

    for (LocaleObserver observer : observers) {
      try {
        LocaleEvent event = new LocaleEvent(observer, locale);
        observer.onLocaleChange(event);
      } catch (Exception e) {
        logger.log(Level.ERROR,
            String.format("Error notifying locale observer: %s", observer.getClass().getName()), e);
      }
    }
  }

  /**
   * Gets the number of currently registered observers.
   *
   * <p>
   * This method is primarily intended for testing and debugging purposes.
   * </p>
   *
   * @return the number of registered observers
   */
  public int getObserverCount() {
    return observers.size();
  }

  /**
   * Clears all registered observers.
   *
   * <p>
   * This method is primarily intended for testing purposes. Use with caution in production code.
   * </p>
   */
  public void clear() {
    if (logger.isLoggable(Level.DEBUG)) {
      logger.log(Level.DEBUG, "Clearing all locale observers");
    }

    observers.clear();
  }
}
