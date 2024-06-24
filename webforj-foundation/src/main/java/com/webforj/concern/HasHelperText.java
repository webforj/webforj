package com.webforj.concern;

import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;

/**
 * An interface that allows components to set and retrieve helper text in a way that makes sense for
 * the specific component.
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @author Hyyan Abo Fakher
 * @since 24.10
 */
public interface HasHelperText<T extends Component> {

  /**
   * Retrieves the helper text of the component.
   *
   * @return the helper text of the component.
   */
  public default String getHelperText() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasHelperText) {
      return ((HasHelperText<?>) component).getHelperText();
    }

    throw new UnsupportedOperationException("The component does not support helper text");
  }

  /**
   * Sets the helper text of the component.
   *
   * @param helperText the helper text to set for the component.
   * @return the component itself after configuring the helper text.
   */
  public default T setHelperText(String helperText) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasHelperText) {
      ((HasHelperText<?>) component).setHelperText(helperText);
      return (T) this;
    }

    throw new UnsupportedOperationException("The component does not support helper text");
  }
}
