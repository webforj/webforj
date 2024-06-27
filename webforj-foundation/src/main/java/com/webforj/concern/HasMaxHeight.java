package com.webforj.concern;

import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;

/**
 * An interface that allows components to set and retrieve maximum height.
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @since 24.10
 */
public interface HasMaxHeight<T extends Component> {

  /**
   * Sets the maximum height of the component.
   *
   * @param maxHeight the maximum height to set for the component.
   * @return the component itself.
   */
  public default T setMaxHeight(String maxHeight) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasMaxHeight) {
      ((HasMaxHeight<?>) component).setMaxHeight(maxHeight);
      return (T) this;
    }

    throw new UnsupportedOperationException("The component does not support max-height property");
  }

  /**
   * Sets the maximum height of the component.
   *
   * @param maxHeight the maximum height to set for the component.
   * @return the component itself.
   *
   * @see #setMaxHeight(String)
   */
  default T setMaxHeight(float maxHeight) {
    return setMaxHeight(String.valueOf(maxHeight) + "px");
  }

  /**
   * Retrieves the maximum height property of the component.
   *
   * @return the maximum height of the component.
   */
  public default String getMaxHeight() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasMaxHeight) {
      return ((HasMaxHeight<?>) component).getMaxHeight();
    }

    throw new UnsupportedOperationException("The component does not support max-height property");
  }

  /**
   * Retrieves the computed maximum height of the component as defined by the browser.
   *
   * @return the computed maximum height of the component.
   */
  public default String getComputedMaxHeight() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasMaxHeight) {
      return ((HasMaxHeight<?>) component).getComputedMaxHeight();
    }

    throw new UnsupportedOperationException("The component does not support max-height property");
  }
}
