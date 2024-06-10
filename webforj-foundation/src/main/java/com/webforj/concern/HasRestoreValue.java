package com.webforj.concern;

import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;

/**
 * An interface for modifying a component's restore value.
 *
 * <p>
 * This interface provides methods to set and get the restore value for a component. It is the text
 * that appears when the user hits the restore key (usually ESC) or by calling the method
 * {@link #restoreValue()}.
 * </p>
 *
 * @param <T> the type of the component that implements this interface.
 * @param <V> the type of the value.
 *
 * @author Hyyan Abo Fakher
 * @since 24.10
 */
public interface HasRestoreValue<T extends Component, V> {

  /**
   * Retrieves the restore value of the component.
   *
   * @return the restore value of the component.
   * @see #setRestoreValue(Object)
   */
  public default V getRestoreValue() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasRestoreValue) {
      return ((HasRestoreValue<?, V>) component).getRestoreValue();
    }

    throw new UnsupportedOperationException(
        "The component does not support the restore value property");
  }

  /**
   * Sets the value of the component to restore when the Escape key is pressed or when the method
   * {@link #restoreValue()} is called.
   *
   * @param value the value to set.
   * @return the component itself.
   */
  public default T setRestoreValue(V value) {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasRestoreValue) {
      ((HasRestoreValue<?, V>) component).setRestoreValue(value);
      return (T) this;
    }

    throw new UnsupportedOperationException(
        "The component does not support the restore value property");
  }

  /**
   * Restores the restore value of the component.
   *
   * @return the component itself.
   * @see #setRestoreValue(Object)
   */
  public default T restoreValue() {
    Component component = ComponentUtil.getBoundComponent(this);

    if (component instanceof HasRestoreValue) {

      ((HasRestoreValue<?, V>) component).restoreValue();
      return (T) this;
    }

    throw new UnsupportedOperationException(
        "The component does not support the restore value property");
  }
}
