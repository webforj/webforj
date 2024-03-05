package com.webforj.concern;

import com.webforj.component.Component;

/**
 * An interface that enables modification of a component's minimum value.
 *
 * <p>
 * This interface provides methods to set and retrieve the minimum possible value for the component.
 * </p>
 *
 * @param <T> the type of the component that implements this interface.
 * @param <V> the type of the minimum value.
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
public interface HasMin<T extends Component, V> {

  /**
   * Sets the minimum possible value for the component.
   *
   * @param min the minimum value to set
   *
   * @return the component itself.
   */
  public T setMin(V min);

  /**
   * Retrieves the minimum possible value for the component.
   *
   * @return the minimum value of the component.
   */
  public V getMin();
}
