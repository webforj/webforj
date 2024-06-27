package com.webforj.concern;

import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;

/**
 * An interface that allows components to set and retrieve width.
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @since 24.10
 * @author Hyyan Abo Fakher
 */
public interface HasWidth<T extends Component> {

  /**
   * Sets the width of the component.
   *
   * @param width the width to set for the component.
   * @return the component itself.
   */
  public default T setWidth(String width) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasWidth) {
      ((HasWidth<?>) component).setWidth(width);
      return (T) this;
    }

    throw new UnsupportedOperationException("The component does not support width property");
  }

  /**
   * Sets the width of the component.
   *
   * @param width the width to set for the component.
   * @return the component itself.
   *
   * @see #setWidth(String)
   */
  default T setWidth(float width) {
    return setWidth(String.valueOf(width) + "px");
  }

  /**
   * Retrieves the width property of the component.
   *
   * <p>
   * This method returns the value of the width property as set through the
   * {@link #setWidth(String)} method. If you want to retrieve the computed width of the component
   * as defined by the browser, use the {@link #getComputedWidth()} method.
   * </p>
   *
   * @return the width of the component.
   */
  public default String getWidth() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasWidth) {
      return ((HasWidth<?>) component).getWidth();
    }

    throw new UnsupportedOperationException("The component does not support width property");
  }

  /**
   * Retrieves the computed width of the component as defined by the browser.
   *
   * @return the computed width of the component.
   */
  public default String getComputedWidth() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasWidth) {
      return ((HasWidth<?>) component).getComputedWidth();
    }

    throw new UnsupportedOperationException("The component does not support width property");
  }
}
