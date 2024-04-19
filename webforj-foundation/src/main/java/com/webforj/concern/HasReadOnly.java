package com.webforj.concern;

import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;
import com.webforj.data.concern.ReadOnlyAware;

/**
 * An interface for implementing methods that allow toggling the read-only status on a component.
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public interface HasReadOnly<T extends Component> extends ReadOnlyAware<T> {

  /**
   * {@inheritDoc}
   */
  @Override
  public default T setReadOnly(boolean readOnly) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasReadOnly) {
      ((HasReadOnly<?>) component).setReadOnly(readOnly);
      return (T) this;
    }

    throw new UnsupportedOperationException(
        "The component does not support the read-only feature.");
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public default boolean isReadOnly() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasReadOnly) {
      return ((HasReadOnly<?>) component).isReadOnly();
    }

    throw new UnsupportedOperationException(
        "The component does not support the read-only feature.");
  }
}
