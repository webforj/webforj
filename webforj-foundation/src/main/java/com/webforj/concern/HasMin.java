package com.webforj.concern;

import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;

/**
 * An interface that enables modification of a component's minimum value.
 *
 * <p>
 * This interface provides methods to set and retrieve the minimum possible value for the component.
 * </p>
 *
 * @param <T> the type of the component that implements this interface.
 * @param <V> the type of the minimum value.
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
public interface HasMin<T extends Component, V> {

  /**
   * Sets the minimum possible value for the component.
   *
   * @param min the minimum value to set
   *
   * @return the component itself.
   */
  public default T setMin(V min) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasMin) {
      ((HasMin<?, V>) component).setMin(min);
      return (T) this;
    }

    throw new UnsupportedOperationException("The component does not support min");
  }

  /**
   * Retrieves the minimum possible value for the component.
   *
   * @return the minimum value of the component.
   */
  public default V getMin() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasMin) {
      return ((HasMin<?, V>) component).getMin();
    }

    throw new UnsupportedOperationException("The component does not support min");
  }
}
