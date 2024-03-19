package com.webforj.concern;

import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;

/**
 * An interface that enables modification of a component's maximum value.
 *
 * <p>
 * This interface provides methods to set and retrieve the maximum possible value for the component.
 * </p>
 *
 * @param <T> the type of the component that implements this interface.
 * @param <V> the type of the maximum value.
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
public interface HasMax<T extends Component, V> {

  /**
   * Sets the maximum possible value for the component.
   *
   * @param max the maximum value to set
   *
   * @return the component itself
   */
  public default T setMax(V max) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasMax) {
      ((HasMax<?, V>) component).setMax(max);
      return (T) this;
    }

    throw new UnsupportedOperationException("The component does not support max");
  }

  /**
   * Retrieves the maximum possible value for the component.
   *
   * @return the maximum value of the component.
   */
  public default V getMax() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasMax) {
      return ((HasMax<?, V>) component).getMax();
    }

    throw new UnsupportedOperationException("The component does not support max");
  }
}
