package com.webforj.concern;

import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;

/**
 * An interface that allows components to set and retrieve maximum width.
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @since 24.10
 */
public interface HasMaxWidth<T extends Component> {

  /**
   * Sets the maximum width of the component.
   *
   * @param maxWidth the maximum width to set for the component.
   * @return the component itself.
   */
  public default T setMaxWidth(String maxWidth) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasMaxWidth) {
      ((HasMaxWidth<?>) component).setMaxWidth(maxWidth);
      return (T) this;
    }

    throw new UnsupportedOperationException("The component does not support max-width property");
  }

  /**
   * Sets the maximum width of the component.
   *
   * @param maxWidth the maximum width to set for the component.
   * @return the component itself.
   *
   * @see #setMaxWidth(String)
   */
  default T setMaxWidth(float maxWidth) {
    return setMaxWidth(String.valueOf(maxWidth) + "px");
  }

  /**
   * Retrieves the maximum width property of the component.
   *
   * @return the maximum width of the component.
   */
  public default String getMaxWidth() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasMaxWidth) {
      return ((HasMaxWidth<?>) component).getMaxWidth();
    }

    throw new UnsupportedOperationException("The component does not support max-width property");
  }

  /**
   * Retrieves the computed maximum width of the component as defined by the browser.
   *
   * @return the computed maximum width of the component.
   */
  public default String getComputedMaxWidth() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasMaxWidth) {
      return ((HasMaxWidth<?>) component).getComputedMaxWidth();
    }

    throw new UnsupportedOperationException("The component does not support max-width property");
  }
}
