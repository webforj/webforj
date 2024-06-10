package com.webforj.concern;

import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;

/**
 * An interface for modifying a component's mask.
 *
 * <p>
 * This interface provides methods to set and retrieve the mask for the component.
 * </p>
 *
 * @param <T> the type of the component that implements this interface.
 * @since 24.10
 */
public interface HasMask<T extends Component> {

  /**
   * Sets the mask for the component.
   *
   * @param mask the mask to set
   * @return the component itself.
   */
  public default T setMask(String mask) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasMask) {
      ((HasMask<?>) component).setMask(mask);
      return (T) this;
    }

    throw new UnsupportedOperationException("The component does not support the mask attribute");
  }

  /**
   * Retrieves the mask for the component.
   *
   * @return the mask for the component.
   * @see #setMask(String)
   */
  public default String getMask() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasMask) {
      return ((HasMask<?>) component).getMask();
    }

    throw new UnsupportedOperationException("The component does not support the mask attribute");
  }
}
