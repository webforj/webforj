package com.webforj.concern;

import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;
import com.webforj.data.validation.client.AutoClientValidation;

/**
 * An interface for modifying a component's auto validation property.
 *
 * <p>
 * This interface provides methods to set and retrieve the auto validation property for a component.
 * </p>
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @since 24.01
 * @author Hyyan Abo Fakher
 */
public interface HasClientAutoValidation<T extends Component> extends AutoClientValidation<T> {

  /**
   * {@inheritDoc}
   */
  @Override
  public default T setAutoClientValidate(boolean autoValidate) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof AutoClientValidation) {
      ((AutoClientValidation<?>) component).setAutoClientValidate(autoValidate);
      return (T) this;
    }

    throw new UnsupportedOperationException(
        "The component does not support the auto validation property");
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public default boolean isAutoClientValidate() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof AutoClientValidation) {
      return ((AutoClientValidation<?>) component).isAutoClientValidate();
    }

    throw new UnsupportedOperationException(
        "The component does not support the auto validation property");
  }
}
