package com.webforj.concern;

import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;

/**
 * An interface for modifying a component's prefix component.
 *
 * <p>
 * This interface provides methods to set and retrieve the prefix component for the component.
 * </p>
 *
 * @param <T> the type of the component that implements this interface.
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public interface HasPrefix<T extends Component> {

  /**
   * Sets the prefix component for the component.
   *
   * <p>
   * The prefix component is the component that is displayed in the component's prefix slot. If a
   * prefix component is already set, then the old prefix component will be destroyed and replaced
   * with the new one.
   * </p>
   *
   * @param prefix the prefix component to set, If prefix is null, the existing prefix component
   *        will be destroyed.
   * @return the component itself.
   */
  public default T setPrefixComponent(Component prefix) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasPrefix) {
      ((HasPrefix<?>) component).setPrefixComponent(prefix);
      return (T) this;
    }

    throw new UnsupportedOperationException(
        "The component does not support setting a prefix component");
  }

  /**
   * Retrieves the prefix component for the component.
   *
   * @return the prefix component for the component.
   */
  public default Component getPrefixComponent() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasPrefix) {
      return ((HasPrefix<?>) component).getPrefixComponent();
    }

    throw new UnsupportedOperationException(
        "The component does not support setting a prefix component");
  }
}
