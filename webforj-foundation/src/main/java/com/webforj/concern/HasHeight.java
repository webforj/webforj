package com.webforj.concern;

import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;

/**
 * An interface that allows components to set and retrieve height.
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @since 24.10
 * @author Hyyan Abo Fakher
 */
public interface HasHeight<T extends Component> {

  /**
   * Sets the height of the component.
   *
   * @param height the height to set for the component.
   * @return the component itself.
   */
  public default T setHeight(String height) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasHeight) {
      ((HasHeight<?>) component).setHeight(height);
      return (T) this;
    }

    throw new UnsupportedOperationException("The component does not support height property");
  }

  /**
   * Sets the height of the component.
   *
   * @param height the height to set for the component.
   * @return the component itself.
   *
   * @see #setHeight(String)
   */
  default T setHeight(float height) {
    return setHeight(String.valueOf(height) + "px");
  }

  /**
   * Retrieves the height property of the component.
   *
   * <p>
   * This method returns the value of the height property as set through the
   * {@link #setHeight(String)} method. If you want to retrieve the computed height of the component
   * as defined by the browser, use the {@link #getComputedHeight()} method.
   * </p>
   *
   * @return the height of the component.
   */
  public default String getHeight() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasHeight) {
      return ((HasHeight<?>) component).getHeight();
    }

    throw new UnsupportedOperationException("The component does not support height property");
  }

  /**
   * Retrieves the computed height of the component as defined by the browser.
   *
   * @return the computed height of the component.
   */
  public default String getComputedHeight() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasHeight) {
      return ((HasHeight<?>) component).getComputedHeight();
    }

    throw new UnsupportedOperationException("The component does not support height property");
  }
}
