package com.webforj.concern;

import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;

/**
 * An interface for modifying a component's placeholder text.
 *
 * <p>
 * This interface provides methods to set and retrieve the placeholder text for the component.
 * </p>
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
public interface HasPlaceholder<T extends Component> {

  /**
   * Sets the placeholder text for the component.
   *
   * @param placeholder the placeholder text to set
   * @return the component itself after setting the placeholder text.
   */
  public default T setPlaceholder(String placeholder) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasPlaceholder) {
      ((HasPlaceholder<?>) component).setPlaceholder(placeholder);
      return (T) this;
    }

    throw new UnsupportedOperationException(
        "The component does not support the placeholder attribute");
  }

  /**
   * Retrieves the placeholder text for the component.
   *
   * @return the placeholder text for the component.
   */
  public default String getPlaceholder() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasPlaceholder) {
      return ((HasPlaceholder<?>) component).getPlaceholder();
    }

    throw new UnsupportedOperationException(
        "The component does not support the placeholder attribute");
  }
}
