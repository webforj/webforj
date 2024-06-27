package com.webforj.concern;

import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;

/**
 * An interface that allows components to set and retrieve minimum width.
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @since 24.10
 */
public interface HasMinWidth<T extends Component> {

  /**
   * Sets the minimum width of the component.
   *
   * @param minWidth the minimum width to set for the component.
   * @return the component itself.
   */
  public default T setMinWidth(String minWidth) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasMinWidth) {
      ((HasMinWidth<?>) component).setMinWidth(minWidth);
      return (T) this;
    }

    throw new UnsupportedOperationException("The component does not support min-width property");
  }

  /**
   * Sets the minimum width of the component.
   *
   * @param minWidth the minimum width to set for the component.
   * @return the component itself.
   *
   * @see #setMinWidth(String)
   */
  default T setMinWidth(float minWidth) {
    return setMinWidth(String.valueOf(minWidth) + "px");
  }

  /**
   * Retrieves the minimum width property of the component.
   *
   * @return the minimum width of the component.
   */
  public default String getMinWidth() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasMinWidth) {
      return ((HasMinWidth<?>) component).getMinWidth();
    }

    throw new UnsupportedOperationException("The component does not support min-width property");
  }

  /**
   * Retrieves the computed minimum width of the component as defined by the browser.
   *
   * @return the computed minimum width of the component.
   */
  public default String getComputedMinWidth() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasMinWidth) {
      return ((HasMinWidth<?>) component).getComputedMinWidth();
    }

    throw new UnsupportedOperationException("The component does not support min-width property");
  }
}
