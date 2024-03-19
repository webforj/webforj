package com.webforj.concern;

import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;

/**
 * An interface for implementing methods to control a component's visibility on a page.
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public interface HasVisibility<T extends Component> {

  /**
   * Checks whether the component is visible or invisible.
   *
   * @return true if the component is visible, false if it's invisible.
   */
  public default boolean isVisible() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasVisibility) {
      return ((HasVisibility<?>) component).isVisible();
    }

    throw new UnsupportedOperationException("The component does not support visibility");
  }

  /**
   * Sets whether the component is visible or invisible.
   *
   * @param visible true to make the component visible, false to make it invisible.
   * @return the component itself.
   */
  public default T setVisible(boolean visible) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasVisibility) {
      ((HasVisibility<?>) component).setVisible(visible);
      return (T) this;
    }

    throw new UnsupportedOperationException("The component does not support visibility");
  }
}
