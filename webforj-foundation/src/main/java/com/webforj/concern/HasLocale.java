package com.webforj.concern;

import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;
import com.webforj.data.LocaleAware;
import java.util.Locale;

/**
 * This interface defines a contract for components that support locale.
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @author Hyyan Abo Fakher
 * @since 24.10
 */
public interface HasLocale<T extends Component> extends LocaleAware<T> {

  /**
   * Gets the locale associated with the component.
   *
   * @return The locale of the component.
   */
  public default Locale getLocale() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasLocale) {
      return ((HasLocale<?>) component).getLocale();
    }

    throw new UnsupportedOperationException("The component does not support locale");
  }

  /**
   * Sets the locale of the component.
   *
   * @param locale The locale to set for the component.
   * @return The component itself.
   */
  public default T setLocale(Locale locale) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasLocale) {
      ((HasLocale<?>) component).setLocale(locale);
      return (T) this;
    }

    throw new UnsupportedOperationException("The component does not support locale");
  }
}
