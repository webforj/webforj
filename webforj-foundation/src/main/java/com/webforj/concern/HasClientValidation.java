package com.webforj.concern;

import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;
import com.webforj.data.validation.client.ClientValidatable;
import com.webforj.data.validation.client.ClientValidator;

/**
 * An interface that enables components to support client-side validation.
 *
 * <p>
 * This interface provides methods for marking a component as invalid, setting an invalid message,
 * and defining a client-side validator. These methods are used to facilitate client-side validation
 * of components, allowing for immediate feedback to the user.
 * </p>
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @since 24.01
 * @author Hyyan Abo Fakher
 */
public interface HasClientValidation<T extends Component> extends ClientValidatable<T> {

  /**
   * {@inheritDoc}
   */
  @Override
  public default T setInvalid(boolean invalid) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasClientValidation) {
      ((HasClientValidation<?>) component).setInvalid(invalid);
      return (T) this;
    }

    throw new UnsupportedOperationException("The component does not support client validation");
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public default boolean isInvalid() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasClientValidation) {
      return ((HasClientValidation<?>) component).isInvalid();
    }

    throw new UnsupportedOperationException("The component does not support client validation");
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public default T setInvalidMessage(String message) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasClientValidation) {
      ((HasClientValidation<?>) component).setInvalidMessage(message);
      return (T) this;
    }

    throw new UnsupportedOperationException("The component does not support client validation");
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public default String getInvalidMessage() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasClientValidation) {
      return ((HasClientValidation<?>) component).getInvalidMessage();
    }

    throw new UnsupportedOperationException("The component does not support client validation");
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public default T setClientValidator(ClientValidator clientValidator) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasClientValidation) {
      ((HasClientValidation<?>) component).setClientValidator(clientValidator);
      return (T) this;
    }

    throw new UnsupportedOperationException("The component does not support client validation");
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public default ClientValidator getClientValidator() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasClientValidation) {
      return ((HasClientValidation<?>) component).getClientValidator();
    }

    throw new UnsupportedOperationException("The component does not support client validation");
  }
}
