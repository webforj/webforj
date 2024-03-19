package com.webforj.concern;

import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;

/**
 * An interface for controlling the enabled state of a component after it has been rendered on the
 * page.
 *
 * <p>
 * This interface provides methods to query and modify the enabled state of a component, allowing
 * you to determine whether it is currently enabled and set its enablement status.
 * </p>
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public interface HasEnablement<T extends Component> {

  /**
   * Checks if the component is currently enabled.
   *
   * @return true if the component is enabled, false if it is disabled
   */
  public default boolean isEnabled() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasEnablement) {
      return ((HasEnablement<?>) component).isEnabled();
    }

    throw new UnsupportedOperationException("The component does not support enablement");
  }

  /**
   * Sets the enablement state of the component.
   *
   * @param enabled true to enable the component, false to disable it
   * @return the component itself.
   */
  public default T setEnabled(boolean enabled) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasEnablement) {
      ((HasEnablement<?>) component).setEnabled(enabled);
      return (T) this;
    }

    throw new UnsupportedOperationException("The component does not support enablement");
  }
}
