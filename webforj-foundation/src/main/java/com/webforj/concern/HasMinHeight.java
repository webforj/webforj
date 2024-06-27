package com.webforj.concern;

import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;

/**
 * An interface that allows components to set and retrieve minimum height.
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @since 24.10
 */
public interface HasMinHeight<T extends Component> {

  /**
   * Sets the minimum height of the component.
   *
   * @param minHeight the minimum height to set for the component.
   * @return the component itself.
   */
  public default T setMinHeight(String minHeight) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasMinHeight) {
      ((HasMinHeight<?>) component).setMinHeight(minHeight);
      return (T) this;
    }

    throw new UnsupportedOperationException("The component does not support min-height property");
  }

  /**
   * Sets the minimum height of the component.
   *
   * @param minHeight the minimum height to set for the component.
   * @return the component itself.
   *
   * @see #setMinHeight(String)
   */
  default T setMinHeight(float minHeight) {
    return setMinHeight(String.valueOf(minHeight) + "px");
  }

  /**
   * Retrieves the minimum height property of the component.
   *
   * @return the minimum height of the component.
   */
  public default String getMinHeight() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasMinHeight) {
      return ((HasMinHeight<?>) component).getMinHeight();
    }

    throw new UnsupportedOperationException("The component does not support min-height property");
  }

  /**
   * Retrieves the computed minimum height of the component as defined by the browser.
   *
   * @return the computed minimum height of the component.
   */
  public default String getComputedMinHeight() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasMinHeight) {
      return ((HasMinHeight<?>) component).getComputedMinHeight();
    }

    throw new UnsupportedOperationException("The component does not support min-height property");
  }
}
