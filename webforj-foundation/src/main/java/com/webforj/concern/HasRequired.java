package com.webforj.concern;

import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;
import com.webforj.data.concern.RequiredAware;

/**
 * An interface that allows components to set and retrieve the required status in a way that makes
 * sense for the specific component.
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @author Hyyan Abo Fakher
 * @since 24.01
 */
public interface HasRequired<T> extends RequiredAware<T> {

  /**
   * {@inheritDoc}
   */
  @Override
  public default T setRequired(boolean required) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof RequiredAware) {
      ((RequiredAware<?>) component).setRequired(required);
      return (T) this;
    }

    throw new UnsupportedOperationException("The component does not support required");
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public default boolean isRequired() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof RequiredAware) {
      return ((RequiredAware<?>) component).isRequired();
    }

    throw new UnsupportedOperationException("The component does not support required");
  }
}
