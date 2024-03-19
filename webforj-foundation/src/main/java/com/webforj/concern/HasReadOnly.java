package com.webforj.concern;

import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;

/**
 * An interface for implementing methods that allow toggling the read-only status on a component.
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public interface HasReadOnly<T extends Component> {

  /**
   * Sets whether a user can edit the component.
   *
   * @param readOnly true to disable editing, false to enable editing.
   * @return the component itself after configuring the read-only status.
   */
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
   * Checks whether the component is set to read-only.
   *
   * @return true if the user cannot edit the component, false if editing is allowed.
   */
  public default boolean isReadOnly() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasReadOnly) {
      return ((HasReadOnly<?>) component).isReadOnly();
    }

    throw new UnsupportedOperationException(
        "The component does not support the read-only feature.");
  }
}
