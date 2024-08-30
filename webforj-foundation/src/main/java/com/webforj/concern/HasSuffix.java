package com.webforj.concern;

import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;

/**
 * An interface for modifying a component's suffix component.
 *
 * <p>
 * This interface provides methods to set and retrieve the suffix component for the component.
 * </p>
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public interface HasSuffix<T extends Component> {

  /**
   * Sets the suffix component for the component.
   *
   * <p>
   * The suffix component is the component that is displayed in the component's suffix slot. If a
   * suffix component is already set, then the old suffix component will be destroyed and replaced
   * with the new one.
   * </p>
   *
   * @param suffix the suffix component to set
   * @return the component itself.
   */
  public default T setSuffixComponent(Component suffix) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasSuffix) {
      ((HasSuffix<?>) component).setSuffixComponent(suffix);
      return (T) this;
    }

    throw new UnsupportedOperationException(
        "The component does not support setting a suffix component");
  }

  /**
   * Retrieves the suffix component for the component.
   *
   * @return the suffix component for the component.
   */
  public default Component getSuffixComponent() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasSuffix) {
      return ((HasSuffix<?>) component).getSuffixComponent();
    }

    throw new UnsupportedOperationException(
        "The component does not support setting a suffix component");
  }
}
