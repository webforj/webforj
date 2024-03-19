package com.webforj.concern;

import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;

/**
 * An interface for modifying the maximum length of a component's value.
 *
 * <p>
 * This interface provides methods to set and retrieve the maximum length allowed for the
 * component's value.
 * </p>
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
public interface HasMaxLength<T extends Component> {

  /**
   * Retrieves the maximum length of the component's value.
   *
   * @return the maximum length
   */
  public default int getMaxLength() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasMaxLength) {
      return ((HasMaxLength<?>) component).getMaxLength();
    }

    throw new UnsupportedOperationException("The component does not support max length");
  }

  /**
   * Sets the maximum length of the component's value.
   *
   * @param maxLength the maximum length to set for the component's value.
   * @return the component itself
   */
  public default T setMaxLength(int maxLength) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasMaxLength) {
      ((HasMaxLength<?>) component).setMaxLength(maxLength);
      return (T) this;
    }

    throw new UnsupportedOperationException("The component does not support max length");
  }
}
