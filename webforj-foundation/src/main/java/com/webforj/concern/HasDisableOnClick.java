package com.webforj.concern;

import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;

/**
 * An interface for components which disable themselves as soon as the user clicks them.
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public interface HasDisableOnClick<T extends Component> {

  /**
   * Checks whether the component is disabled when the user clicks it.
   *
   * @return true if the component is disabled when the user clicks it
   */
  public default boolean isDisableOnClick() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasDisableOnClick) {
      return ((HasDisableOnClick<?>) component).isDisableOnClick();
    }

    throw new UnsupportedOperationException("The component does not support disable on click");
  }

  /**
   * Sets whether the component is disabled as soon as the user clicks it.
   *
   * @param disableOnClick true to disable the component when the user clicks it
   * @return the component itself
   */
  @SuppressWarnings("unchecked")
  public default T setDisableOnClick(boolean disableOnClick) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasDisableOnClick) {
      ((HasDisableOnClick<?>) component).setDisableOnClick(disableOnClick);
      return (T) this;
    }

    throw new UnsupportedOperationException("The component does not support disable on click");
  }
}
