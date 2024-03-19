package com.webforj.concern;

import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;

/**
 * An interface for modifying the minimum length of a component's value.
 *
 * <p>
 * This interface provides methods to set and retrieve the minimum length required for the
 * component's value.
 * </p>
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
public interface HasMinLength<T extends Component> {

  /**
   * Retrieves the minimum length required for the component's value.
   *
   * @return the minimum length required for the component's value.
   */
  public default int getMinLength() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasMinLength) {
      return ((HasMinLength<?>) component).getMinLength();
    }

    throw new UnsupportedOperationException("The component does not support the min length");
  }

  /**
   * Sets the minimum length required for the component's value.
   *
   * @param minLength the minimum length to set for the component's value
   * @return the component itself.
   */
  public default T setMinLength(int minLength) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasMinLength) {
      ((HasMinLength<?>) component).setMinLength(minLength);
      return (T) this;
    }

    throw new UnsupportedOperationException("The component does not support the min length");
  }
}
