package com.webforj.concern;

import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;

/**
 * An interface that allows components to set and retrieve autofocus property.
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @author Hyyan Abo Fakher
 * @since 24.21
 */
public interface HasAutoFocus<T extends Component> {

  /**
   * Checks if the component should automatically have focus when the app has finished loading.
   *
   * @return true if the component should automatically have focus when the app has finished
   *         loading.
   */
  public default boolean isAutoFocus() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasAutoFocus) {
      return ((HasAutoFocus<?>) component).isAutoFocus();
    }

    throw new UnsupportedOperationException("The component does not support autofocus property");
  }

  /**
   * When true, the component should automatically have focus when the app has finished loading.
   *
   * @param autofocus true to automatically have focus when the app has finished loading.
   * @return the component itself
   */
  public default T setAutoFocus(boolean autofocus) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasAutoFocus) {
      ((HasAutoFocus<?>) component).setAutoFocus(autofocus);
      return (T) this;
    }

    throw new UnsupportedOperationException("The component does not support autofocus property");
  }
}
