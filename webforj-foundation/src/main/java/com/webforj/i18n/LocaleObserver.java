package com.webforj.i18n;

import com.webforj.i18n.event.LocaleEvent;
import java.io.Serializable;

/**
 * An interface for components that want to be notified when the application locale changes.
 *
 * <p>
 * Components implementing this interface will be automatically registered to receive locale change
 * notifications when they are created and unregistered when they are destroyed. The registration
 * and unregistration process is handled automatically by the component lifecycle.
 * </p>
 *
 * <p>
 * This interface is separate from {@link com.webforj.concern.HasLocale}. HasLocale provides
 * per-component locale storage, while LocaleObserver provides a reactive pattern for responding to
 * application-wide locale changes. Components can implement one, both, or neither depending on
 * their needs.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.10
 */
@FunctionalInterface
public interface LocaleObserver extends Serializable {

  /**
   * Called when the application locale changes.
   *
   * <p>
   * This method is invoked for all registered observers whenever {@link com.webforj.App#setLocale}
   * is called with a new locale value.
   * </p>
   *
   * @param event the locale change event containing the new locale and the component
   */
  void onLocaleChange(LocaleEvent event);
}
