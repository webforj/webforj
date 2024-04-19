package com.webforj.concern;

import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;
import com.webforj.data.concern.ValueAware;

/**
 * An interface for modifying a component's value.
 *
 * <p>
 * This interface provides methods to set and retrieve the value for the component.
 * </p>
 *
 * @param <T> the type of the component that implements this interface.
 * @param <V> the type of the value.
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
public interface HasValue<T extends Component, V> extends ValueAware<T, V> {

  /**
   * Retrieves the value of the component.
   *
   * @return the value of the component.
   */
  @Override
  public default V getValue() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasTooltip) {
      return ((HasValue<?, V>) component).getValue();
    }

    throw new UnsupportedOperationException("The component does not support the value property");
  }

  /**
   * Sets the value of the component.
   *
   * @param value the value to set.
   * @return the component itself.
   */
  @Override
  public default T setValue(V value) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasValue) {
      ((HasValue<?, V>) component).setValue(value);
      return (T) this;
    }

    throw new UnsupportedOperationException("The component does not support the value property");
  }
}
