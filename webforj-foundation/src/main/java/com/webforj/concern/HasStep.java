package com.webforj.concern;

import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;

/**
 * An interface for modifying a component's step value.
 *
 * <p>
 * This interface provides methods to set and retrieve the step value for the component.
 * </p>
 *
 * @param <T> the type of the component that implements this interface.
 * @param <V> the type of the step value.
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
public interface HasStep<T extends Component, V> {

  /**
   * Retrieves the step value of the component.
   *
   * @return the step value of the component.
   */
  public default V getStep() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasStep) {
      return ((HasStep<?, V>) component).getStep();
    }

    throw new UnsupportedOperationException("The component does not support the step property");
  }

  /**
   * Sets the step value of the component.
   *
   * @param step the step value to set.
   * @return the component itself.
   */
  public default T setStep(V step) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasStep) {
      ((HasStep<?, V>) component).setStep(step);
      return (T) this;
    }

    throw new UnsupportedOperationException("The component does not support the step property");
  }
}
