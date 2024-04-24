package com.webforj.concern;

import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;
import com.webforj.data.validation.client.AutoClientValidationOnLoad;

/**
 * An interface for modifying a component's auto validation on load property.
 *
 * <p>
 * This interface provides methods to set and retrieve the auto validation on load property for a
 * component.
 * </p>
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @author Hyyan Abo Fakher
 * @since 24.01
 */
public interface HasClientAutoValidationOnLoad<T extends Component>
    extends AutoClientValidationOnLoad<T> {

  /**
   * {@inheritDoc}
   */
  @Override
  public default T setAutoClientValidateOnLoad(boolean autoValidateOnLoad) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof AutoClientValidationOnLoad) {
      ((AutoClientValidationOnLoad<?>) component).setAutoClientValidateOnLoad(autoValidateOnLoad);
      return (T) this;
    }

    throw new UnsupportedOperationException(
        "The component does not support the auto validation on load property");
  }


  /**
   * {@inheritDoc}
   */
  @Override
  public default boolean isAutoClientValidateOnLoad() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof AutoClientValidationOnLoad) {
      return ((AutoClientValidationOnLoad<?>) component).isAutoClientValidateOnLoad();
    }

    throw new UnsupportedOperationException(
        "The component does not support the auto validation on load property");
  }
}
